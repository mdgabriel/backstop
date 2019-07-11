module Utils (pairs
             , unpairs
             , root
             , dot
             , dotdot
             , reduceFilePath
             , absoluteLink
             , relativeLink
             , compElems
             , subdirectories
             , subdirectory
             , aDirectory
             , aSymbolicLink
             , anObject
             , putOrPageStrLn
             )
where

import Control.Exception  (IOException, try)
import Control.Monad      (unless)
import Data.List          (nub,intersect,isPrefixOf,tails,foldl')
import Data.Maybe         (fromJust,isNothing)
import System.Environment (getEnvironment)
import System.Exit        (ExitCode(..))
import System.FilePath    (pathSeparator,joinPath,(</>),isAbsolute,splitDirectories)
import System.IO          (hClose,hFlush,hPutStr)
import System.Posix.Files (getSymbolicLinkStatus,isSymbolicLink,isDirectory)
import System.Process     (waitForProcess,createProcess,shell,StdStream(..),CreateProcess(..))


-- Section 0: exported items---------------------------------------------------------

-- | 'root' = @[pathSeparator]@
root   :: FilePath
root    = [pathSeparator]

-- | 'dot' = \".\"
dot    :: FilePath
dot     = "."

-- | 'dotdot' = \"..\"
dotdot :: FilePath
dotdot  = ".."

-- | Given a list @[a]@, 'pairs' returns the list of all pairs
-- @[(a,a)]@ of @[a]@ progressing from the head to tail of @[a]@.  For
-- example,
-- 
-- > pairs [x1,x2,x3,x4]
-- > == [(x1,x2),(x1,x3),(x1,x4),(x2,x3),(x2,x4),(x3,x4)]
--
-- Note that
--
-- > pairs []     == []
-- > pairs (x:[]) == []

pairs :: [a] -> [(a,a)]
pairs []     = []
pairs [x]    = []
pairs xs     =
    let pair_ x0 = map (\x1 -> (x0,x1))
        xss = init $ init $ tails xs
    in concatMap (\xs -> pair_ (head xs) (tail xs)) xss

-- | For @length l /= 1@,
-- 
-- > (unpairs . pairs) l == l
-- 
-- is @True@.
--
-- > Number of elements    Number of pairs
-- > ------------------    ---------------
-- >                  0 => 0
-- >                  1 => 0
-- >                  2 => 1
-- >                  3 => 3
-- >                  4 => 6
-- >                  5 => 10
-- >                  e => p = e(e-1)/2, e >= 0

unpairs :: Eq a => [(a,a)] -> [a]
unpairs ps
    | null ps   = []
    | otherwise = fst (head ps) : foldr ((:).snd) [] (take (elements 0 (length ps) - 1) ps)
    where
    elements e p = if p == e*(e-1) `div` 2 then e else elements (e+1) p

-- | 'reduceFilePath' returns a pathname that is reduced to canonical
-- form equivalent to that of ksh(1), that is, symbolic link names are
-- treated literally when finding the directory name.  See @cd -L@ of
-- ksh(1).  Specifically, extraneous separators @(\"/\")@, dot
-- @(\".\")@, and double-dot @(\"..\")@ directories are removed.

reduceFilePath :: FilePath -> FilePath
reduceFilePath = joinPath . filePathComponents

-- | Given a pathname @wd@ representing a workng directory and two
-- pathnames @p1@ and @p2@, 'absoluteLink' returns the reduced
-- ('reduceFilePath') and absolute path to @p2@ to which @p1@ could point
-- as a symbolic link.
-- 
-- If @wd@ is not an absolute path, it is assumed to be a relative
-- path that is relative to the root directory of the file system tree
-- and is made an absolute path.

absoluteLink :: FilePath -> FilePath -> FilePath -> FilePath
absoluteLink wd _ = reduceFilePath . ((pathSeparator:wd)</>)

-- | Given a pathname @wd@ representing a workng directory and two
-- pathnames @p1@ and @p2@, 'relativeLink' returns the reduced
-- ('reduceFilePath') and relative path from @p1@ to @p2@ to which
-- @p1@ could point as a symbolic link.  Thus, @p1@ must not be the
-- root @(\"/\")@, dot @(\".\")@, nor double-dot @(\"..\")@
-- directories but of the form @\"\/SomePath\/SymLink\"@
-- 
-- If @wd@ is not an absolute path, it is assumed to be a relative
-- path that is relative to the root directory of the file system tree
-- and is made an absolute path.

relativeLink :: FilePath -> FilePath -> FilePath -> FilePath
relativeLink wd p1 p2 =
    let wd_           = pathSeparator:wd
        ab p          = if isAbsolute p then p else wd_ </> p
        (_, p1_, p2_) = decomposeFilePaths (ab p1) (ab p2)
        p1_s          = map (const dotdot) $ splitDirectories p1_
        p1__          = (joinPath . (if null p1_s then id else tail)) p1_s
        p             = p1__ </> p2_
    in if null p then dot else p

-- | 'compElems' compares elements of two lists and returns the list of
-- elements that are in both lists, unique to the first list, and
-- uniqued to the second list.  Duplicates are 'nub'ed.

compElems :: Eq a => [a] -> [a] -> ([a], [a], [a])
compElems [] l2 = ([], [], nub l2)
compElems l1 [] = ([], nub l1, [])
compElems l1 l2 =
    let l1_     = nub l1
        l2_     = nub l2
        same    = intersect l1_ l2_
        unique1 = filter (`notElem` same) l1_
        unique2 = filter (`notElem` same) l2_
    in (same, unique1, unique2)

-- | 'subdirectories' returns true if either the absolute pathname
-- @p1@ or the absolute pathname @p2@ is a sub-directory or sub-path
-- of the other.

subdirectories :: FilePath -> FilePath -> Bool
subdirectories p1 p2 = subdirectory p1 p2 || subdirectory p2 p1

-- | 'subdirectory' returns true if the absolute pathname @p1@ is a
-- subdirectory of the absolute pathname @p2@ as in
-- 
-- > p1 `subdirectory` p2
-- 
-- otherwise false.

subdirectory ::  FilePath -> FilePath -> Bool
subdirectory p1 p2 =
        let x = filePathComponents p1
            y = filePathComponents p2
        in isPrefixOf y x

-- | 'aDirectory' returns true if @p@ is not a symbolic link
-- and is a directory, otherwise false.

aDirectory :: FilePath -> IO Bool
aDirectory =
    fmap (either (const False :: IOException -> Bool) isDirectory) . try . getSymbolicLinkStatus

-- | 'aSymbolicLink' returns true if @p@ is a symbolic link, otherwise
-- false.

aSymbolicLink :: FilePath -> IO Bool
aSymbolicLink =
    fmap (either (const False :: IOException -> Bool) isSymbolicLink) . try . getSymbolicLinkStatus

-- | 'anObject' returns true if @p@ is an object in the filesystem,
-- otherwise false.  Symbolic links are not deferenced.

anObject :: FilePath -> IO Bool
anObject =
    fmap (either (const False :: IOException -> Bool) (const True)) . try . getSymbolicLinkStatus


-- | Put a string or page it if the environmental variable PAGER is set.

putOrPageStrLn :: String -> IO ExitCode
putOrPageStrLn str
    | null str  = return ExitSuccess
    | otherwise =
    do pager <- fmap (lookup "PAGER") getEnvironment
       if isNothing pager
           then putStrLn str >> return ExitSuccess
           else do (inh, _, _, pid) <- createProcess (shell $ fromJust pager){std_in = CreatePipe}
                   unless (isNothing inh) $ do hPutStr (fromJust inh) str
                                               hFlush (fromJust inh)
                                               hClose (fromJust inh)
                   waitForProcess pid


-- Section 1 ------------------------------------------------------------------------

-- | Given two abosulte pathnames @p1@ and @p2@, 'decomposeFilePaths'
-- returns the common prefix to @p1@ and @p2@ plus the remainders of
-- @p1@ and @p2@, otherwise null plus @p1@ and @p2@.
decomposeFilePaths :: FilePath -> FilePath -> (FilePath, FilePath, FilePath)
decomposeFilePaths p1 p2
    | null p1   =  ([], p1, p2)
    | null p2   =  ([], p1, p2)
    | otherwise =
    let p1s = filePathComponents p1
        p2s = filePathComponents p2
        (pxs_, p1s_, p2s_) = decomposeFilePaths_ ([], p1s, p2s)
        decomposeFilePaths_ (pxs,  [], p2s) = (pxs,  [], p2s)
        decomposeFilePaths_ (pxs, p1s,  []) = (pxs, p1s,  [])
        decomposeFilePaths_ (pxs, p1s, p2s) =
            let p1h = head p1s
                p2h = head p2s
                p1t = tail p1s
                p2t = tail p2s
            in if p1h == p2h
               then decomposeFilePaths_ (pxs++[p1h], p1t, p2t)
               else (pxs, p1s, p2s)
    in (joinPath pxs_, joinPath p1s_, joinPath p2s_)


-- Section 2 ------------------------------------------------------------------------

-- | 'filePathComponents' returns pathname components that are reduced
-- to canonical form equivalent to that of ksh(1), that is, symbolic
-- link names are treated literally when finding the directory name.
-- See @cd -L@ of ksh(1).  Specifically, extraneous separators
-- @(\"/\")@, dot @(\".\")@, and double-dot @(\"..\")@ directories are
-- removed.

filePathComponents :: FilePath -> [FilePath]
filePathComponents p
    | null p    = [dot]
    | otherwise = let c  = head p
                      cs = tail p
                  in reverse $ snd $ foldl accumulate
                         (if c == pathSeparator then ([],[root]) else ([c],[dot]))
                         (cs++root)
    where
    accumulate :: (String,[String]) -> Char -> (String,[String])
    accumulate (cs, css) c =
        if c == pathSeparator
        then ([],(if null cs then id else cons cs) css)
        else (cs++[c],css)
    cons :: String -> [String] -> [String]
    cons cs css
        | cs == dot = css
        | css == [dot] = [cs]
        | cs /= dotdot || null css = cs : css
        | otherwise =
          let hd = head css
              tl = tail css
          in if hd == root
             then css
             else if hd == dotdot
                  then cs : css
                  else if null tl
                       then [dot]
                       else tl
