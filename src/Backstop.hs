{-# LANGUAGE FlexibleContexts #-}
module Backstop (backstop) where

import Control.Applicative ((<$>))
import Control.Exception (try)
import Control.Monad.Reader (ask,ReaderT,runReaderT,withReaderT)
import Control.Monad.State.Strict (put,lift,when,evalStateT,get,StateT,runStateT,filterM)
import Control.Monad.Trans (liftIO)
import Data.List ((\\),sort,delete)
import Data.Monoid --(Monoid(..))
import Environment (Environment(..),defaultEnv)
import System.Exit (exitWith,ExitCode(..))
import System.FilePath ((</>),isAbsolute)
import System.Directory (createDirectory,removeDirectory,removeFile,getDirectoryContents)
import System.IO (stderr,hPutStrLn)
import System.IO.Error hiding (try)
import System.Posix.Files (readSymbolicLink,isDirectory,isSymbolicLink,createSymbolicLink,getSymbolicLinkStatus)
import Utils (aDirectory,compElems,absoluteLink,relativeLink,subdirectory,dot,dotdot,anObject)


-- Section 0: The interface to Main.runWith: Backstop.backstop ----------------------

-- | 'backstop' the 'Environment' reporting a summary if requested,
-- any errors, and a return code.  The return code is either 0 for
-- success, the number of errors, or 255 for infinity.

backstop :: Environment -> IO ()
backstop e =
    do bs <- runReaderT (if deBackstop e then _unbackstop else _backstop) e
       when (report e) $ putBackstop bs
       exitWith $ if errs bs > 0
                  then ExitFailure (if errs bs < 251 then errs bs else 255)
                  else ExitSuccess


-- Section 1 ------------------------------------------------------------------------

data Backstop =
    BS { calls    :: Int      -- ^ Number of calls to 'backstop'
       , depth    :: Int      -- ^ Depth of recursion of 'backstop'
       , breadth  :: Int      -- ^ Breadth for the given call to and depth of 'backstop'
       , symlinks :: Int      -- ^ Number of symlinks created or removed
       , dirs     :: Int      -- ^ Number of directories created or removed
       , errs     :: Int      -- ^ Number of non-fatal IO errors that occurred
       } deriving Eq

instance Semigroup Backstop where
    bs1 <> bs2 = let add        f = f bs1 + f bs2
                     biggest    f = max (f bs1) (f bs2)
                 in BS (add calls)
                       (add depth)
                       (biggest breadth)
                       (add symlinks)
                       (add dirs)
                       (add errs)

instance Monoid Backstop where
-- Because of the use of biggest below, this instance of mempty is
-- only an identity of a monoid for all Int >= 0.  This is the case in
-- the Backstop module.  The monoid laws do not hold for Int < 0.
    mempty = BS 0 0 0 0 0 0

-- | Put to STDERR the 'Backstop' results 'symlinks', 'dirs', 'calls',
-- 'depth', 'breadth', and 'errs'.

putBackstop :: Backstop -> IO ()
putBackstop bs =
    hPutStrLn stderr ("Symlinks: " ++ (justify 5.show.symlinks) bs ++ "; " ++
                      "Dirs: "     ++ (justify 4.show.dirs)     bs ++ "; " ++
                      "Calls: "    ++ (justify 4.show.calls)    bs ++ "; " ++
                      "Depth: "    ++ (justify 4.show.depth)    bs ++ "; " ++
                      "Breadth: "  ++ (justify 3.show.breadth)  bs ++ "; " ++
                      "Errs: "     ++ (justify 3.show.errs)     bs
                     )
    where
    justify w s = let l = length s in (if l < w then (replicate (w-l) ' ' ++) else id) s


-- Section 2 ------------------------------------------------------------------------

-- | '_backstop' the 'Environment' keeping track of the 'Backstop' results.

_backstop :: ReaderT Environment IO Backstop
_backstop =
    do e <- ask
       if (length.operands) e < 2
           then return oneCall
           else do bs1 <- __backstop
                   bs2 <- withReaderT nextSource _backstop
                   return (bs1 `mappend` bs2)
    where
    __backstop =
        do e <- ask
           let t = operands e !! 0
           let s = operands e !! 1
           (bs1, ts) <- listDirectoryContents t
           (bs2, ss) <- listDirectoryContents s
           let (ps12, _, ps2) = compElems ts ss
           bs3 <- createObjects ps2
           ds  <- listDirectories (ps12++ps2)
           bs4 <- mconcat <$> mapM (\d -> withReaderT (nextDepth d) __backstop) ds
           (return.mconcat) [bs4, bs3, bs2, bs1, oneCall, if null ds then  mempty else oneDepth]

-- | Given an 'Environment' and a list of pathnames @ps@, create the
-- objects which are links from the target to the source or
-- directories as required yielding the 'Backstop' result.

createObjects :: [FilePath] -> ReaderT Environment IO Backstop
createObjects = fmap mconcat . mapM createObject

-- | 'createObject' attempts to create a symbolic link or directory
-- given an 'Environment' and a basename pathname @p@ yielding
-- the 'Backstop' result.

createObject :: FilePath -> ReaderT Environment IO Backstop
createObject p =
    do e <- ask
       if (length.operands) e < 2 then return mempty else _createObject p
    where
    prt p t s = liftIO $ putStr $ if p
                                  then "mkdir " ++ "\"" ++ t ++ "\"\n"
                                  else "ln -s " ++ "\"" ++ s ++ "\" " ++ "\"" ++ t ++ "\"\n"
    _createObject p =
        do e <- ask
           let d         = pwd e
           let t         = (operands e !! 0) </> p
           let s         = (operands e !! 1) </> p
           let s_        = (if absolute e then absoluteLink else relativeLink) d t s
           p <- liftIO $ fmap (populate e &&)  (aDirectory s)
           let one = if p then oneDir else oneSymlink
           when (trace e) $ prt p t s_
           if noAction e
               then return one
               else do (bs, _) <- tryOperation $ if p
                                                 then createDirectory t
                                                 else createSymbolicLink s_ t
                       return $ if bs /= mempty then bs else one


-- Section 3 ------------------------------------------------------------------------

data DirectoryContentStatus = Occupied -- ^ Directory occupied with remaining content after '__unbackstop'
                            | Emptied  -- ^ Directory was emptied by '__unbackstop'
                            | Empty    -- ^ Directory was empty when viewed by '__unbackstop'
                              deriving Eq

instance Semigroup DirectoryContentStatus where
    dcs1 <> dcs2 = case (dcs1, dcs2) of
                        (Occupied, _) -> Occupied
                        (_, Occupied) -> Occupied
                        (Empty, _)    -> Empty
                        (_, Empty)    -> Empty
                        (_,_)         -> Emptied

instance Monoid DirectoryContentStatus where
    mempty = Emptied

type DCS = DirectoryContentStatus -- Short version

-- | '_unbackstop' the 'Environment' keeping track of the 'Backstop' results.

_unbackstop :: ReaderT Environment IO Backstop
_unbackstop =
    do e <- ask
       if (length.operands) e < 2
           then return oneCall
           else do -- Emptied when "empty" directory, otherwise Empty or set Occupied
                   bs1 <- evalStateT __unbackstop Emptied
                   bs2 <- withReaderT nextSource _unbackstop
                   return (bs1 `mappend` bs2)
    where
    __unbackstop :: StateT DCS (ReaderT Environment IO) Backstop
    __unbackstop =
        do e <- ask
           let t = operands e !! 0
           (bs1, ts) <- lift $ listDirectoryContents t
           ds  <- lift $ listDirectories ts
           let os = ts \\ ds
           bs2 <- destroyObjects os
           if null ds
               then do put $ if symlinks bs2 /= length os then Occupied
                             else if null os then Empty else Emptied
                       (return.mconcat) [bs2, bs1, oneCall]
               else do bs3 <- mconcat <$> mapM recurse ds
                       dcs <- get
                       put $ dcs `mappend` if symlinks bs2 /= length os then Occupied
                                           else if null os then Empty else Emptied
                       (return.mconcat) [bs3, bs2, bs1, oneCall, oneDepth]

    recurse :: FilePath -> StateT DCS (ReaderT Environment IO) Backstop
    recurse d = do s1 <- get
                   -- Emptied when "empty" directory, otherwise Empty or set Occupied
                   (bs1, s2) <- lift $ withReaderT (nextDepth d) (runStateT __unbackstop Emptied)
                   put s2
                   bs2 <- destroyObjects [d]
                   s3 <- get
                   put (mconcat [s1, s2, s3])
                   (return.mconcat) [bs2, bs1]

    {- Assume StateT monads for ms, then
    
       sequence ms = foldr k (return []) ms
           where
           k mx mxs =
               StateT $ \s ->
                   (runState mx) s >>= \ (x, s') ->
                       (runStateT mxs) s' >>= \ (xs, s'') -> (x:xs, s'')

       Since mapM f xs is sequence (map f xs), it is recurse plus (dcs
       <- get) in __unbackstop above via mapM (sequence) that threads
       the DCS state value through __unbackstop.  All put and get
       calls are in encapsulated in __unbackstop and all that it calls
       and only calls: destroyObjects, destroyObject, destroySymlink,
       and destroyDir.  For now, only destroyDir has a get and a put.
    -}

-- | Given an 'Environment' and a list of pathnames @ps@, destroy the
-- objects which are links from the target to the source or
-- directories as required yielding the 'Backstop' result.

destroyObjects :: [FilePath] -> StateT DCS (ReaderT Environment IO) Backstop
destroyObjects = fmap mconcat . mapM destroyObject

-- | 'destroyObject' attempts to destroy a symbolic link or directory
-- given an 'Environment' and a basename pathname @p@ yielding
-- the 'Backstop' result.

destroyObject :: FilePath -> StateT DCS (ReaderT Environment IO) Backstop
destroyObject p =
    do e <- ask
       if (length.operands) e < 2 then return mempty else _destroyObject p
    where
    _destroyObject p =
        do e <- ask
           let trg = (operands e !! 0) </> p
           (bs, fs) <- lift $ tryOperation (getSymbolicLinkStatus trg)
           if bs /= mempty
               then return bs
               else if isSymbolicLink fs
                        then destroySymlink p
                        else if isDirectory fs
                                 then destroyDir p
                                 else return mempty

-- | 'destroySymlink' attempts to destroy a symbolic link given an
-- 'Environment' and a basename pathname @p@ yielding the 'Backstop'
-- result.

destroySymlink :: FilePath -> StateT DCS (ReaderT Environment IO) Backstop
destroySymlink p =
    do e <- ask
       if (length.operands) e < 2 then return mempty else _destroySymlink p
    where
    prt sym   = liftIO $ putStr ("rm -f " ++ "\"" ++ sym ++ "\"\n")
    abs d p f = (if isAbsolute f then id else (d</>).(p</>)) f
    rm e sym  = do when (trace e) $ prt sym
                   if noAction e
                       then return oneSymlink
                       else do (bs, _) <- lift $ tryOperation (removeFile sym)
                               return $ if bs /= mempty then bs else oneSymlink
    _destroySymlink p =
        do e <- ask
           let cwd    = pwd e
           let trg    = operands e !! 0
           let src    = operands e !! 1
           let sym    = trg </> p
           (bs, p) <- lift $ tryOperation (readSymbolicLink sym)
           if bs /= mempty
               then return bs
               else if not $ abs cwd trg p `subdirectory` abs cwd "" src
                        then return mempty
                        else rm e sym

-- | Given an 'Environment', remove an empty target directory @d@ if
-- and only if the corresonding directory path exists under the source
-- directory in which case the target directory is considered to have
-- been populated by the source directory yielding the 'Backstop'
-- result.

destroyDir :: FilePath -> StateT DCS (ReaderT Environment IO) Backstop
destroyDir p =
    do e <- ask
       if (length.operands) e < 2 || (not.populate) e then return mempty else _destroyDir p
    where
    prt dir = liftIO $ putStr ("rmdir \"" ++ dir ++"\"\n")
    _destroyDir p =
        do e <- ask
           let trg = (operands e !! 0) </> p
           let src = (operands e !! 1) </> p
           sDir   <- liftIO $ aDirectory src
           dcs    <- get
           if ((dcs == Empty) && not sDir) || (dcs == Occupied)
               then return mempty
               else do when (trace e) $ prt trg
                       if noAction e
                           then return oneDir
                           else do (bs, _) <- lift $ tryOperation (removeDirectory trg)
                                   if bs /= mempty
                                       then do { put Occupied; return bs }
                                       else do { put Emptied; return oneDir }


-- Section 4 ------------------------------------------------------------------------

-- | Given an 'Environment' and a pathname @p@,
-- 'listDirectoryContents' yields the sorted list @ps@ of directory
-- contents without the dot @(\".\")@ or double @(\"..\")@ directories
-- yielding the 'Backstop' result @bs@ in @(bs, ps)@.

listDirectoryContents :: FilePath -> ReaderT Environment IO (Backstop, [FilePath])
listDirectoryContents p =
    do directory <- liftIO $ aDirectory p
       if directory
           then do (bs, ps) <- tryOperation (sort . delete dotdot . delete dot <$> getDirectoryContents p)
                   return $ if bs /= mempty then (bs, []) else (mempty{breadth = length ps}, ps)
           else return (mempty, [])

-- | 'listDirectories' of @ps@ for further backstopping by '_backstop'.

listDirectories :: [FilePath] -> ReaderT Environment IO [FilePath]
listDirectories ps =
    let dir e p =
            let t = (operands e !! 0) </> p
                s = (operands e !! 1) </> p
            in do tDir <- aDirectory t
                  sDir <- aDirectory s
                  tObj <- anObject   t
                  return $ if deBackstop e then tDir
                           else if noAction e && populate e && not tObj
                                then sDir else tDir && sDir
    in do e <- ask
          if (length.operands) e < 2  then return [] else liftIO $ filterM (dir e) ps


-- Section 5 ------------------------------------------------------------------------

-- | Given an 'Environment', 'IO.try' the operation @IO a@ yielding
-- @(Backstop, a)@.

tryOperation :: IO a -> ReaderT Environment IO (Backstop, a)
tryOperation io =
    liftIO (try io) >>=
        either (\ioe -> putIOE ioe >> return (oneErr, error "N/A")) (\x -> return (mempty, x))

-- | Given an 'Environment', 'putIOE' prints an error message for
-- @ioe@ to STDERR.

putIOE :: IOError -> ReaderT Environment IO ()
putIOE ioe =
    do e <- ask
       let ln = ioeGetLocation ioe
       let fn = ioeGetFileName ioe
       let es = ioeGetErrorString ioe
       let s  = command e ++ ": " ++ ln ++ ": " ++ maybe es (++": "++es) fn
       liftIO $ hPutStrLn stderr s


-- Section 6 ------------------------------------------------------------------------

-- | Return the new 'Environment' given a pathname @p@ or the original
-- 'Environment' if done.

nextDepth :: FilePath -> Environment -> Environment
nextDepth p e =
    if (length.operands) e < 2
    then e
    else let t = operands e !! 0
             s = operands e !! 1
         in e{operands=[t </> p, s </> p]}


-- | Return the next 'Environment' to process or the original
-- 'Environment' if done.

nextSource :: Environment -> Environment
nextSource e =
    if (length.operands) e < 2
    then e
    else e{operands=(head.operands) e : (tail.tail.operands) e}


-- Section 7 ------------------------------------------------------------------------

-- | Set 'calls' to one in 'mempty'.

oneCall    :: Backstop
oneCall    = mempty{calls = 1}

-- | Set 'depth' to one in 'mempty'.

oneDepth   :: Backstop
oneDepth   = mempty{depth = 1}

-- | Set 'symlinks' to one in 'mempty'.

oneSymlink :: Backstop
oneSymlink = mempty{symlinks = 1}

-- | Set 'dirs' to one in 'mempty'.

oneDir     :: Backstop
oneDir     = mempty{dirs = 1}

-- | Set 'errs' to one in 'mempty'.

oneErr     :: Backstop
oneErr     = mempty{errs = 1}
