module Manual(putManual) where

import System.Exit (exitWith)
import Utils (putOrPageStrLn)

-- | Display the internal manual.

putManual :: IO ()
putManual = putOrPageStrLn internalManual >>= exitWith

-- | Internal manual.  The definitive source of the internal manual is
-- the description section of the module Main.

internalManual :: String
internalManual =
    "backstop(1)                 User Commands                  backstop(1)\n"++
    "\n"++
    "NAME\n"++
    "   backstop - Backstop a target directory by source directories\n"++
    "\n"++
    "SYNOPSIS\n"++
    "   backstop [-nprt] [-a|-d] trgdir srcdir [srcdir ...]\n"++
    "   backstop -h\n"++
    "   backstop -l\n"++
    "   backstop -m\n"++
    "   backstop -v\n"++
    "   backstop -y\n"++
    "\n"++
    "AVAILABILITY\n"++
    "   Marcus D. Gabriel (c)1999-2017.  All rights reserved.\n"++
    "\n"++
    "   marcus@gabriel.name\n"++
    "\n"++
    "   License terms: GNU GPL version 3 or later (backstop -l)\n"++
    "\n"++
    "DESCRIPTION\n"++
    "   Backstop a target directory by source directories thereby\n"++
    "   creating a backing chain using relative symbolic links by default\n"++
    "   or absolute symbolic links by option.  If more than one source\n"++
    "   directory is given, backstop the target directory with each\n"++
    "   source directory in turn starting from left to right on the\n"++
    "   command line.\n"++
    "\n"++
    "   Intuitively, backstop fills in the missing objects or holes that\n"++
    "   exist in the target directory relative to the source directories\n"++
    "   thereby creating a merged view of the target and source\n"++
    "   directories.\n"++
    "\n"++
    "   De-backstop a target directory by its source directories.  In\n"++
    "   this case, the source directories no longer need to exist.  If\n"++
    "   more than one source directory is given, de-backstop the target\n"++
    "   directory by the source directories.\n"++
    "\n"++
    "   Symbolic links are never followed.  However, the target and\n"++
    "   source directories are canonicalized (realpath(3)) internally by\n"++
    "   the command backstop before backstopping and de-backstoppoing.\n"++
    "\n"++
    "   By default no output is displayed except possibly to STDERR.\n"++
    "\n"++
    "ALGORITHM\n"++
    "   There must be at least two arguments or operands.  The first\n"++
    "   operand is the target directory, and the second operand and\n"++
    "   beyond are source directories.  The target and source directory\n"++
    "   must be parallel directories, that is, neither canonicalized\n"++
    "   (realpath(3)) directory path may be a sub-directory of the other.\n"++
    "   If the above is not true, a usage error occurs.\n"++
    "\n"++
    "   Nevertheless, pathnames are internally in reduced form such that\n"++
    "   symbolic link names are treated literally when finding directory\n"++
    "   names.  See \"cd -L\" of ksh(1).  In other words, components in the\n"++
    "   pathname that are not directories but symbolic links to\n"++
    "   directories are not resolved to their corresponding directories.\n"++
    "\n"++
    "   A list of objects or names is collected under the root of the\n"++
    "   source directory excluding the directory dot (.) and the\n"++
    "   directory double-dot (..).  If the list is empty, the algorithm\n"++
    "   is done, otherwise the objects divide into two classes, objects\n"++
    "   that are not directories and objects that are directories.  There\n"++
    "   are five possibilities:\n"++
    "\n"++
    "   1. The object is either dot (.) or double-dot (..) whereupon no\n"++
    "      action is taken.\n"++
    "\n"++
    "   2. If an object of the same name as the name of the object under\n"++
    "      the root of the source directory does not exists under the\n"++
    "      root of the target directory, then a symbolic link, relative\n"++
    "      or absolute as required, is created under the root of the\n"++
    "      target directory to the object under the root of the source\n"++
    "      directory.  If populating (-p) and the object under the root\n"++
    "      of the source directory is a directory, then a sub-directory\n"++
    "      is created instead.\n"++
    "\n"++
    "   3. If an object of the same name as the name of the object under\n"++
    "      the root of the source directory does exists under the root of\n"++
    "      the target directory and is a file, no action is taken,\n"++
    "      regardless of whether or not the object under the root of the\n"++
    "      source directory is a file or a directory.\n"++
    "\n"++
    "   4. If an object of the same name as the name of the object under\n"++
    "      the root of the source directory does exists under the root of\n"++
    "      the target directory and is a directory, and if the object of\n"++
    "      the same name under the root of the source directory is a\n"++
    "      file, no action is taken.\n"++
    "\n"++
    "   5. If an object of the same name as the name of the object under\n"++
    "      the root of the source directory does exists under the root of\n"++
    "      the target directory and is a directory, and if the object of\n"++
    "      the same name under the root of the source directory is also a\n"++
    "      directory, then recursion occurs with the objects of the same\n"++
    "      name under the roots of the target and source directories as\n"++
    "      new root target and source directories, respectively.\n"++
    "\n"++
    "   When de-backstopping a target directory by source directories,\n"++
    "   the sources no longer need to exist.  Symbolic links under the\n"++
    "   target are removed if and only if they point to a pahtname under\n"++
    "   one of the source directories.  This can be determined even if\n"++
    "   the source directory no longer exists because symbolic links\n"++
    "   within directory pathnames are taken literally.  See \"cd -L\" of\n"++
    "   ksh(1).\n"++
    "\n"++
    "   When de-populating while de-backstopping (-pd), a directory is\n"++
    "   removed if all objects within the directory are symbolic links\n"++
    "   that have been remove while de-backstopping.  An empty directory\n"++
    "   is removed if and only if there exists a corresponding directory\n"++
    "   path under one of the source directories as under the target\n"++
    "   directory.  This can only be determined if the source directory\n"++
    "   and correponding sub-directory still exist.  If not, then no\n"++
    "   action is taken.\n"++
    "\n"++
    "OPTIONS\n"++
    "   The following options are supported:\n"++
    "\n"++
    "   -a Create symbolic links which are absolute paths.\n"++
    "\n"++
    "   -d De-backstop the target directory by its source directories.\n"++
    "      The -a option has no affect on the -d option.\n"++
    "\n"++
    "   -h Display a short command summary.\n"++
    "\n"++
    "   -l Display the backstop license terms.\n"++
    "\n"++
    "   -m Display this internal manual page.\n"++
    "\n"++
    "   -n Take no action but trace possible program execution.  The -n\n"++
    "      option implies the -t option.\n"++
    "\n"++
    "   -p Populate the target by making equivalent sub-directories of\n"++
    "      the source sub-directories instead of symbolic links for these\n"++
    "      sub-directories that point back into the sources.  For\n"++
    "      example, if the target directory is empty, then for a set of\n"++
    "      source directories, from left to right on the command line,\n"++
    "      this creates a combined directory structure of the sources\n"++
    "      under the target directory in which all non-directories are\n"++
    "      symbolic links pointing back into the source directories.\n"++
    "      This is similar to the lndir(1) command of the X consortium.\n"++
    "\n"++
    "      With the -d option, all sub-directories emptied of symbolic\n"++
    "      links under the target directory are removed, and all empty\n"++
    "      directories are removed if and only if the correspoding\n"++
    "      sub-directory under the source directory exists.\n"++
    "\n"++
    "   -r Report the number of symbolic links and directories created,\n"++
    "      the number of calls and maximum depth of backstop, the\n"++
    "      maximum breadth observed by backstop, and the number of non-\n"++
    "      fatal errors encountered.  With the -d option, show the\n"++
    "      number of symbolic links and directories removed.\n"++
    "\n"++
    "   -t Show the trace of program execution.  The output is a list\n"++
    "      of executable commands.\n"++
    "\n"++
    "   -v The version of backstop.\n"++
    "\n"++
    "   -y A synopsis of the internal manual page.\n"++
    "\n"++
    "OPERANDS\n"++
    "   trgdir\n"++
    "      The directory that will be backed by other directories.  The\n"++
    "      target directory can be backed by multiple source directories\n"++
    "      where each source directory could be backed by yet another\n"++
    "      directory or multiple directories thereby creating an\n"++
    "      extensible backing chain.\n"++
    "\n"++
    "   srcdir\n"++
    "      The directory which backs a target directory.  The source\n"++
    "      directory can be backed by multiple additional directories\n"++
    "      where each additional directory could be backed by yet another\n"++
    "      directory or multiple directories thereby creating an\n"++
    "      extensible backing chain.\n"++
    "\n"++
    "USAGE NOTES\n"++
    "   Evolved with ghc 6.10, 6.12, 7.0, 7.4, 7.10, 8.0.2, and 8.4.1.\n"++
    "   Tested with 8.6.5.\n"++
    "\n"++
    "EXAMPLES\n"++
    "   To display a summary of usage, use\n"++
    "\n"++
    "      backstop -h\n"++
    "\n"++
    "   To display a synopsis of this internal manual page, use\n"++
    "\n"++
    "      backstop -y\n"++
    "\n"++
    "   To display this internal manual page, use\n"++
    "\n"++
    "      backstop -m\n"++
    "\n"++
    "   To trace program execution, use\n"++
    "\n"++
    "      backstop -t /export/install/sparc/devlopment/modules \\\n"++
    "                  /export/install/sparc/modules\n"++
    "\n"++
    "   To trace program execution taking no action, use\n"++
    "\n"++
    "      backstop -n /export/install/sparc/devlopment/modules \\\n"++
    "                  /export/install/sparc/modules\n"++
    "\n"++
    "   To back /export/install/sparc/devlopment/modules with\n"++
    "   /export/install/sparc/modules using relative symbolic links, use\n"++
    "\n"++
    "      backstop /export/install/sparc/devlopment/modules \\\n"++
    "               /export/install/sparc/modules\n"++
    "\n"++
    "   To back /export/install/sparc/devlopment/modules with\n"++
    "   /export/install/sparc/modules using absolute symbolic links, use\n"++
    "\n"++
    "      backstop -a /export/install/sparc/devlopment/modules \\\n"++
    "                  /export/install/sparc/modules\n"++
    "\n"++
    "   To stop backing /export/install/sparc/devlopment/modules with\n"++
    "   /export/install/sparc/modules, use\n"++
    "\n"++
    "      backstop -d /export/install/sparc/devlopment/modules \\\n"++
    "                  /export/install/sparc/modules\n"++
    "\n"++
    "ENVIRONMENT VARIABLES\n"++
    "   PAGER   Page the internal manual page or license terms\n"++
    "           using \"${PAGER}\", otherwise print it.\n"++
    "\n"++
    "EXIT STATUS\n"++
    "   An exit status of 0 is returned if successful, otherwise non-zero\n"++
    "   is returned.  The following error exit codes are used by\n"++
    "   backstop:\n"++
    "\n"++
    "       EXIT CODE    MEANING\n"++
    "          251       Less than two operands.\n"++
    "          252       Not a directory, that is, trgdir or one or more\n"++
    "                    of the srcdir are not a directory.\n"++
    "          253       Not parallel directories, that is, a srcdir is\n"++
    "                    a sub-directory of trgdir or vice versa, or one\n"++
    "                    of the srcdir is a sub-directory of another\n"++
    "                    srcdir.\n"++
    "          254       Not yet implemented.\n"++
    "          255       Non-fatal errors occurred.  Error messages are\n"++
    "                    sent to STDERR for each non-fatal error that\n"++
    "                    occurred with return code equal to the number of\n"++
    "                    non-falal errors if less than 251, otherwise the\n"++
    "                    return code is 255, that is, \"infinity\".\n"++
    "\n"++
    "FILES\n"++
    "   N/A.\n"++
    "\n"++
    "SEE ALSO\n"++
    "   ln(1), mkdir(1), rm(1), rmdir(1), ksh(1), lndir(1), realpath(3)\n"++
    "\n"++
    "   \"Source Control to Project Control: Applying RCS and SCCS\" by Don\n"++
    "   Bolinger and Tan Bronson. ISBN 1-56592-117-8.\n"++
    "\n"++
    "NOTES\n"++
    "   Please send bug reports to marcus@gabriel.name.\n"++
    "\n"++
    "BUGS\n"++
    "   No known bugs to date.\n"
