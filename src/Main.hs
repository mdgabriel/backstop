
{- |
> backstop(1)                 User Commands                  backstop(1)
> 
> NAME
>    backstop - Backstop a target directory by source directories
> 
> SYNOPSIS
>    backstop [-nprt] [-a|-d] trgdir srcdir [srcdir ...]
>    backstop -h
>    backstop -l
>    backstop -m
>    backstop -v
>    backstop -y
> 
> AVAILABILITY
>    Marcus D. Gabriel (c)1999-2017.  All rights reserved.
> 
>    marcus@gabriel.name
> 
>    License terms: GNU GPL version 3 or later (backstop -l)
> 
> DESCRIPTION
>    Backstop a target directory by source directories thereby
>    creating a backing chain using relative symbolic links by default
>    or absolute symbolic links by option.  If more than one source
>    directory is given, backstop the target directory with each
>    source directory in turn starting from left to right on the
>    command line.
> 
>    Intuitively, backstop fills in the missing objects or holes that
>    exist in the target directory relative to the source directories
>    thereby creating a merged view of the target and source
>    directories.
> 
>    De-backstop a target directory by its source directories.  In
>    this case, the source directories no longer need to exist.  If
>    more than one source directory is given, de-backstop the target
>    directory by the source directories.
> 
>    Symbolic links are never followed.  However, the target and
>    source directories are canonicalized (realpath(3)) internally by
>    the command backstop before backstopping and de-backstoppoing.
> 
>    By default no output is displayed except possibly to STDERR.
> 
> ALGORITHM
>    There must be at least two arguments or operands.  The first
>    operand is the target directory, and the second operand and
>    beyond are source directories.  The target and source directory
>    must be parallel directories, that is, neither canonicalized
>    (realpath(3)) directory path may be a sub-directory of the other.
>    If the above is not true, a usage error occurs.
> 
>    Nevertheless, pathnames are internally in reduced form such that
>    symbolic link names are treated literally when finding directory
>    names.  See "cd -L" of ksh(1).  In other words, components in the
>    pathname that are not directories but symbolic links to
>    directories are not resolved to their corresponding directories.
> 
>    A list of objects or names is collected under the root of the
>    source directory excluding the directory dot (.) and the
>    directory double-dot (..).  If the list is empty, the algorithm
>    is done, otherwise the objects divide into two classes, objects
>    that are not directories and objects that are directories.  There
>    are five possibilities:
> 
>    1. The object is either dot (.) or double-dot (..) whereupon no
>       action is taken.
> 
>    2. If an object of the same name as the name of the object under
>       the root of the source directory does not exists under the
>       root of the target directory, then a symbolic link, relative
>       or absolute as required, is created under the root of the
>       target directory to the object under the root of the source
>       directory.  If populating (-p) and the object under the root
>       of the source directory is a directory, then a sub-directory
>       is created instead.
> 
>    3. If an object of the same name as the name of the object under
>       the root of the source directory does exists under the root of
>       the target directory and is a file, no action is taken,
>       regardless of whether or not the object under the root of the
>       source directory is a file or a directory.
> 
>    4. If an object of the same name as the name of the object under
>       the root of the source directory does exists under the root of
>       the target directory and is a directory, and if the object of
>       the same name under the root of the source directory is a
>       file, no action is taken.
> 
>    5. If an object of the same name as the name of the object under
>       the root of the source directory does exists under the root of
>       the target directory and is a directory, and if the object of
>       the same name under the root of the source directory is also a
>       directory, then recursion occurs with the objects of the same
>       name under the roots of the target and source directories as
>       new root target and source directories, respectively.
> 
>    When de-backstopping a target directory by source directories,
>    the sources no longer need to exist.  Symbolic links under the
>    target are removed if and only if they point to a pahtname under
>    one of the source directories.  This can be determined even if
>    the source directory no longer exists because symbolic links
>    within directory pathnames are taken literally.  See "cd -L" of
>    ksh(1).
> 
>    When de-populating while de-backstopping (-pd), a directory is
>    removed if all objects within the directory are symbolic links
>    that have been remove while de-backstopping.  An empty directory
>    is removed if and only if there exists a corresponding directory
>    path under one of the source directories as under the target
>    directory.  This can only be determined if the source directory
>    and correponding sub-directory still exist.  If not, then no
>    action is taken.
> 
> OPTIONS
>    The following options are supported:
> 
>    -a Create symbolic links which are absolute paths.
> 
>    -d De-backstop the target directory by its source directories.
>       The -a option has no affect on the -d option.
> 
>    -h Display a short command summary.
> 
>    -l Display the backstop license terms.
> 
>    -m Display this internal manual page.
> 
>    -n Take no action but trace possible program execution.  The -n
>       option implies the -t option.
> 
>    -p Populate the target by making equivalent sub-directories of
>       the source sub-directories instead of symbolic links for these
>       sub-directories that point back into the sources.  For
>       example, if the target directory is empty, then for a set of
>       source directories, from left to right on the command line,
>       this creates a combined directory structure of the sources
>       under the target directory in which all non-directories are
>       symbolic links pointing back into the source directories.
>       This is similar to the lndir(1) command of the X consortium.
> 
>       With the -d option, all sub-directories emptied of symbolic
>       links under the target directory are removed, and all empty
>       directories are removed if and only if the correspoding
>       sub-directory under the source directory exists.
> 
>    -r Report the number of symbolic links and directories created,
>       the number of calls and maximum depth of backstop, the
>       maximum breadth observed by backstop, and the number of non-
>       fatal errors encountered.  With the -d option, show the
>       number of symbolic links and directories removed.
> 
>    -t Show the trace of program execution.  The output is a list
>       of executable commands.
> 
>    -v The version of backstop.
> 
>    -y A synopsis of the internal manual page.
> 
> OPERANDS
>    trgdir
>       The directory that will be backed by other directories.  The
>       target directory can be backed by multiple source directories
>       where each source directory could be backed by yet another
>       directory or multiple directories thereby creating an
>       extensible backing chain.
> 
>    srcdir
>       The directory which backs a target directory.  The source
>       directory can be backed by multiple additional directories
>       where each additional directory could be backed by yet another
>       directory or multiple directories thereby creating an
>       extensible backing chain.
> 
> USAGE NOTES
>    Evolved with ghc 6.10, 6.12, 7.0, 7.4, 7.10, 8.0.2, and 8.4.1.
>    Tested with 8.4.1.
> 
> EXAMPLES
>    To display a summary of usage, use
> 
>       backstop -h
> 
>    To display a synopsis of this internal manual page, use
> 
>       backstop -y
> 
>    To display this internal manual page, use
> 
>       backstop -m
> 
>    To trace program execution, use
> 
>       backstop -t /export/install/sparc/devlopment/modules \
>                   /export/install/sparc/modules
> 
>    To trace program execution taking no action, use
> 
>       backstop -n /export/install/sparc/devlopment/modules \
>                   /export/install/sparc/modules
> 
>    To back /export/install/sparc/devlopment/modules with
>    /export/install/sparc/modules using relative symbolic links, use
> 
>       backstop /export/install/sparc/devlopment/modules \
>                /export/install/sparc/modules
> 
>    To back /export/install/sparc/devlopment/modules with
>    /export/install/sparc/modules using absolute symbolic links, use
> 
>       backstop -a /export/install/sparc/devlopment/modules \
>                   /export/install/sparc/modules
> 
>    To stop backing /export/install/sparc/devlopment/modules with
>    /export/install/sparc/modules, use
> 
>       backstop -d /export/install/sparc/devlopment/modules \
>                   /export/install/sparc/modules
> 
> ENVIRONMENT VARIABLES
>    PAGER   Page the internal manual page or license terms
>            using "${PAGER}", otherwise print it.
> 
> EXIT STATUS
>    An exit status of 0 is returned if successful, otherwise non-zero
>    is returned.  The following error exit codes are used by
>    backstop:
> 
>        EXIT CODE    MEANING
>           251       Less than two operands.
>           252       Not a directory, that is, trgdir or one or more
>                     of the srcdir are not a directory.
>           253       Not parallel directories, that is, a srcdir is
>                     a sub-directory of trgdir or vice versa, or one
>                     of the srcdir is a sub-directory of another
>                     srcdir.
>           254       Not yet implemented.
>           255       Non-fatal errors occurred.  Error messages are
>                     sent to STDERR for each non-fatal error that
>                     occurred with return code equal to the number of
>                     non-falal errors if less than 251, otherwise the
>                     return code is 255, that is, "infinity".
> 
> FILES
>    N/A.
> 
> SEE ALSO
>    ln(1), mkdir(1), rm(1), rmdir(1), ksh(1), lndir(1), realpath(3)
> 
>    "Source Control to Project Control: Applying RCS and SCCS" by Don
>    Bolinger and Tan Bronson. ISBN 1-56592-117-8.
> 
> NOTES
>    Please send bug reports to marcus@gabriel.name.
> 
> BUGS
>    No known bugs to date.
-}

module Main(main) where


-- Section: imports
import Backstop (backstop)
import Control.Exception (try,IOException(..))
import Control.Monad (when,unless,filterM)
import Data.List (intercalate)
import Environment (Environment(..),defaultEnv)
import License (putLicense)
import Manual (putManual)
import System.Console.GetOpt (ArgDescr(..),OptDescr(..),ArgOrder(..),usageInfo,getOpt)
import System.Directory (getCurrentDirectory,canonicalizePath)
import System.Environment (getEnvironment,getProgName,getArgs)
import System.Exit (exitSuccess,exitWith,ExitCode(..))
import System.FilePath (takeFileName)
import System.IO (stderr,stdout,hPutStrLn,Handle(..))
import Utils (reduceFilePath,pairs,subdirectories,aDirectory)

-- Section: main: No need to change this section.
{- | Backstop a target directory by source directories.  Refer to
'backstop'@ -h@, 'backstop'@ -y@, or 'backstop'@ -m@ for more information.
-}
main :: IO ()
main =
  do cmdLine  <- getArgs
     progName <- getProgName
     case getOpt (argOrder config) options cmdLine of
       (opts, args, []) -> runCmd $ foldl updateEnvField env opts
           where env = defaultEnv{command=takeFileName progName,operands=args}
       (_, _, errs) -> error $ concat errs ++ usageInfo (header progName) options


-- Section: summary, help, version: No need to change this section.
{- |
Given the programe name, returns the program name and the release
version.
-}
versionHeader :: String -> String
versionHeader progName = takeFileName progName ++ " release " ++ release config

{- |
Given the program name, returns a synopis of options.
-}
header :: String -> String
header progName = versionHeader (takeFileName progName) ++
                  "\nusage: " ++ takeFileName progName ++ flagsOps config

{- |
Puts the program name and its version.
-}
putVersion :: IO ()
putVersion = do progName <- getProgName
                putStrLn (versionHeader progName)
                exitSuccess

{- |
Puts the program name and the synopsis of options to handle (stdout or
stderr) with the supplied the exit code.
-}
putSynopsis :: Handle -> String -> ExitCode -> IO ()
putSynopsis h s e = do progName <- getProgName
                       hPutStrLn h (takeFileName progName ++ ": " ++ s ++ header progName)
                       exitWith e

{- |
Puts the program name and a short summary.
-}
putSummary :: IO ()
putSummary = do progName <- getProgName
                putStrLn (usageInfo (header progName) options)
                exitSuccess


-- Section: configuration (to be configured)
{- |
Configuration information is initial, constant information, that is,
not needing updates.
-}
data Configuration =
    Config {
            argOrder       :: ArgOrder Flag,
            release        :: String,
            flagsOps       :: String
           }

{- |
To be configured once with the versions number and synopsis.  Other
items may of course be added to the configuration as needed.
-}
config :: Configuration
config =
    Config {
            argOrder       = RequireOrder,
            -- Update revision number: toggle case
            release        = "1.3.0.352",
            flagsOps       = " {-{h|l|m|v|y}|[-nprt] [-a|-d] trgdir srcdir [srcdir...]}"
           }
    where
    revision =
        let num = flip elem ['0'..'9']
        in  reverse.dropWhile (not.num).reverse.dropWhile (not.num)


-- Section: options (to be configured)
{- |
Flags set as a function of the command line options supplied.
-}
data Flag = Absolute | DeBackstop | Help | License |
            Manual | NoAction | Populate | Report | Trace |
            Synopsis | Version

{- |
Options that can be given on the command line.
-}
options :: [OptDescr Flag]
options =
    [
     Option "a" ["absolute"]    (NoArg Absolute)   "Make absolute symbolic links",
     Option "d" ["de-backstop"] (NoArg DeBackstop) "De-backstop target given sources",
     Option "h" ["help"]        (NoArg Help)       "Help summary",
     Option "l" ["license"]     (NoArg License)    "License terms",
     Option "m" ["manual"]      (NoArg Manual)     "Manual page",
     Option "n" ["no-action"]   (NoArg NoAction)   "No-action trace",
     Option "p" ["populate"]    (NoArg Populate)   "Populate by making sub-directories",
     Option "r" ["report"]      (NoArg Report)     "Report final statistics",
     Option "t" ["trace"]       (NoArg Trace)      "Trace program execution",
     Option "v" ["version"]     (NoArg Version)    "Version of backstop",
     Option "y" ["synopsis"]    (NoArg Synopsis)   "Help synopsis"
    ]


-- Section: updating a field in the environment (to be configured)
{- |
Every option, and only an option, must be able to be updated here.
-}
updateEnvField :: Environment -> Flag -> Environment
updateEnvField env opt =
    case opt of
      Absolute   -> env{absolute    = True}
      DeBackstop -> env{deBackstop  = True}
      Help       -> env{help        = True}
      License    -> env{license     = True}
      Manual     -> env{manual      = True}
      NoAction   -> env{noAction    = True}
      Populate   -> env{populate    = True}
      Report     -> env{report      = True}
      Trace      -> env{trace       = True}
      Synopsis   -> env{synopsis    = True}
      Version    -> env{version     = True}
--    _          -> error (command env ++ ": updateEnvField: unknown error")


-- Section: running the command given the environment (to be configured)
{- |

No need to change the function runCmd.

Update the environment with updateEnv, verify the environment with
verifyEnv, resolve option conflicts with resolveEnv, and run the
command given the environemnt with runWith which does the actual work
where the thre reall usually imported from a module.
-}
runCmd :: Environment -> IO ()
runCmd env
    | synopsis env = putSynopsis stdout "\r" ExitSuccess
    | help    env  = putSummary
    | version env  = putVersion
    | manual  env  = putManual
    | license env  = putLicense
    | otherwise    =
      do env_ <- updateEnv env
         verifyEnv env_
         runWith (resolveEnv env_)

{- |
'updateEnv' updates the environment ('Environment') from the universe,
the external environment.
-}
updateEnv :: Environment -> IO Environment
updateEnv env = do cd <- getCurrentDirectory
                   es <- getEnvironment
                   os <- mapM canonicalizePath (operands env)
                   let pd = lookup "PWD" es
                       e' = maybe env{pwd = cd} (\pd -> env{pwd = pd}) pd
                   return e'{operands = os}

{- |
'verifyEnv' verifies the environment created from the command line
options and the supplied operands.
-}
verifyEnv :: Environment -> IO ()
verifyEnv env = verify1 env >> verify2 env >> verify3 env
  where
    verify1 env =
        when (length (operands env) < 2) $
             putSynopsis stderr "less than two operands\n" (ExitFailure 251)
    verify2 env =
        do let ops = if deBackstop env then [(head.operands) env] else operands env
           ps <- filterM (fmap not . aDirectory) ops
           let errMsg s = putSynopsis stderr (s++(unwords ps++"\n")) (ExitFailure 252)
           unless (null ps) $
               errMsg $ if length ps == 1 then "not a directory: " else "not directories: "
    verify3 env =
        do ds <- mapM canonicalizePath (operands env)
           let ps     = filter (uncurry subdirectories) $ pairs ds
           let errMsg = intercalate "\n    " $ map (\ (x,y) -> "("++x++","++y++")") ps
           unless (null ps) $
               putSynopsis stderr ("not parallel directories:\n    "++errMsg++"\n") (ExitFailure 253)

{- |
'resolveEnv' resolves the environment created from the command line
options and the supplied operands, e.g., if an option over-rides
another, 'resolveEnv' takes this into account.
-}
resolveEnv :: Environment -> Environment
-- Order is important, so please do not change it without study
resolveEnv = resolveEnv2 . resolveEnv1
    where
      -- The -n option implies -t
      resolveEnv1 env =
          if noAction env
          then env{trace = True}
          else env
      -- Reduce directories, that is, ksh(1) canonical form
      resolveEnv2 env = env{operands = map reduceFilePath (operands env)}

{- |
'runWith' this environment.  'runWith' is the interface between the command
line options plus operands and the real work. 'runWith' is 'backstop'.
-}
runWith :: Environment -> IO ()
runWith = backstop
