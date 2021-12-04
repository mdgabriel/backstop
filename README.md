# The backstop command

The backstop command is a UNIX, Linux, and *BSD tool. Intuitively,
backstop fills in the missing objects or holes that exist in the
target directory relative to the source directories thereby creating a
merged view of the target and source directories.  De-backstop undoes
this.  See "backstop -m" for a manual page or the
[homepage](https://github.com/mdgabriel/backstop.git) for more
information.

The command backstop was created over twenty years ago to manage
development environement toolchains in which part of the project was
under source control and part of the project was not, e.g. binaries.
It was inspired by lndir, graft, and "Source Control to Project
Control: Applying RCS and SCCS" by Don Bolinger and Tan Bronson.
Techniques have changed with time, but backstop is a robust tool
with an extensive test suite that runs around 2700 tests, a
combination of HUnit, QuickCheck, and black box tests.

In the author's obviously biased veiw, backstop has pedagogical
value.  The module Backstop uses a ReaderT monad transformer to
manage an Environment passed to it via the command line, a StateT
monad transformer to keep track of a set of counters and to keep track
of the status of directory content while backstopping or
de-backstopping.  The last two are Monoids.  With this structure,
the handling of a dry-run for backstop or de-backstop is rather
powerful even it is a bit subtle (obscure) to understand.

On a personal note, the author truly began to appreciate Haskell after
creating the Backstop module and its test suite and has not stopped
appreciating Haskell since.

# The backstop manual page

```backstop(1)                 User Commands                  backstop(1)

NAME
   backstop - Backstop a target directory by source directories

SYNOPSIS
   backstop [-nprt] [-a|-d] trgdir srcdir [srcdir ...]
   backstop -h
   backstop -l
   backstop -m
   backstop -v
   backstop -y

AVAILABILITY
   Marcus D. Gabriel (c)1999-2019.  All rights reserved.

   marcus@gabriel.name

   License terms: GNU GPL version 3 or later (backstop -l)

DESCRIPTION
   Backstop a target directory by source directories thereby
   creating a backing chain using relative symbolic links by default
   or absolute symbolic links by option.  If more than one source
   directory is given, backstop the target directory with each
   source directory in turn starting from left to right on the
   command line.

   Intuitively, backstop fills in the missing objects or holes that
   exist in the target directory relative to the source directories
   thereby creating a merged view of the target and source
   directories.

   De-backstop a target directory by its source directories.  In
   this case, the source directories no longer need to exist.  If
   more than one source directory is given, de-backstop the target
   directory by the source directories.

   Symbolic links are never followed.  However, the target and
   source directories are canonicalized (realpath(3)) internally by
   the command backstop before backstopping and de-backstoppoing.

   By default no output is displayed except possibly to STDERR.

ALGORITHM
   There must be at least two arguments or operands.  The first
   operand is the target directory, and the second operand and
   beyond are source directories.  The target and source directory
   must be parallel directories, that is, neither canonicalized
   (realpath(3)) directory path may be a sub-directory of the other.
   If the above is not true, a usage error occurs.

   Nevertheless, pathnames are internally in reduced form such that
   symbolic link names are treated literally when finding directory
   names.  See "cd -L" of ksh(1).  In other words, components in the
   pathname that are not directories but symbolic links to
   directories are not resolved to their corresponding directories.

   A list of objects or names is collected under the root of the
   source directory excluding the directory dot (.) and the
   directory double-dot (..).  If the list is empty, the algorithm
   is done, otherwise the objects divide into two classes, objects
   that are not directories and objects that are directories.  There
   are five possibilities:

   1. The object is either dot (.) or double-dot (..) whereupon no
      action is taken.

   2. If an object of the same name as the name of the object under
      the root of the source directory does not exists under the
      root of the target directory, then a symbolic link, relative
      or absolute as required, is created under the root of the
      target directory to the object under the root of the source
      directory.  If populating (-p) and the object under the root
      of the source directory is a directory, then a sub-directory
      is created instead.

   3. If an object of the same name as the name of the object under
      the root of the source directory does exists under the root of
      the target directory and is a file, no action is taken,
      regardless of whether or not the object under the root of the
      source directory is a file or a directory.

   4. If an object of the same name as the name of the object under
      the root of the source directory does exists under the root of
      the target directory and is a directory, and if the object of
      the same name under the root of the source directory is a
      file, no action is taken.

   5. If an object of the same name as the name of the object under
      the root of the source directory does exists under the root of
      the target directory and is a directory, and if the object of
      the same name under the root of the source directory is also a
      directory, then recursion occurs with the objects of the same
      name under the roots of the target and source directories as
      new root target and source directories, respectively.

   When de-backstopping a target directory by source directories,
   the sources no longer need to exist.  Symbolic links under the
   target are removed if and only if they point to a pahtname under
   one of the source directories.  This can be determined even if
   the source directory no longer exists because symbolic links
   within directory pathnames are taken literally.  See "cd -L" of
   ksh(1).

   When de-populating while de-backstopping (-pd), a directory is
   removed if all objects within the directory are symbolic links
   that have been remove while de-backstopping.  An empty directory
   is removed if and only if there exists a corresponding directory
   path under one of the source directories as under the target
   directory.  This can only be determined if the source directory
   and correponding sub-directory still exist.  If not, then no
   action is taken.

OPTIONS
   The following options are supported:

   -a Create symbolic links which are absolute paths.

   -d De-backstop the target directory by its source directories.
      The -a option has no affect on the -d option.

   -h Display a short command summary.

   -l Display the backstop license terms.

   -m Display this internal manual page.

   -n Take no action but trace possible program execution.  The -n
      option implies the -t option.

   -p Populate the target by making equivalent sub-directories of
      the source sub-directories instead of symbolic links for these
      sub-directories that point back into the sources.  For
      example, if the target directory is empty, then for a set of
      source directories, from left to right on the command line,
      this creates a combined directory structure of the sources
      under the target directory in which all non-directories are
      symbolic links pointing back into the source directories.
      This is similar to the lndir(1) command of the X consortium.

      With the -d option, all sub-directories emptied of symbolic
      links under the target directory are removed, and all empty
      directories are removed if and only if the correspoding
      sub-directory under the source directory exists.

   -r Report the number of symbolic links and directories created,
      the number of calls and maximum depth of backstop, the
      maximum breadth observed by backstop, and the number of non-
      fatal errors encountered.  With the -d option, show the
      number of symbolic links and directories removed.

   -t Show the trace of program execution.  The output is a list
      of executable commands.

   -v The version of backstop.

   -y A synopsis of the internal manual page.

OPERANDS
   trgdir
      The directory that will be backed by other directories.  The
      target directory can be backed by multiple source directories
      where each source directory could be backed by yet another
      directory or multiple directories thereby creating an
      extensible backing chain.

   srcdir
      The directory which backs a target directory.  The source
      directory can be backed by multiple additional directories
      where each additional directory could be backed by yet another
      directory or multiple directories thereby creating an
      extensible backing chain.

USAGE NOTES
   Evolved with ghc 6.10, 6.12, 7.0, 7.4, 7.10, 8.0.2, 8.4.1,
   8.6.5, and 8.10.5.  Tested with 8.10.5.

EXAMPLES
   To display a summary of usage, use

      backstop -h

   To display a synopsis of this internal manual page, use

      backstop -y

   To display this internal manual page, use

      backstop -m

   To trace program execution, use

      backstop -t /export/install/sparc/devlopment/modules \
                  /export/install/sparc/modules

   To trace program execution taking no action, use

      backstop -n /export/install/sparc/devlopment/modules \
                  /export/install/sparc/modules

   To back /export/install/sparc/devlopment/modules with
   /export/install/sparc/modules using relative symbolic links, use

      backstop /export/install/sparc/devlopment/modules \
               /export/install/sparc/modules

   To back /export/install/sparc/devlopment/modules with
   /export/install/sparc/modules using absolute symbolic links, use

      backstop -a /export/install/sparc/devlopment/modules \
                  /export/install/sparc/modules

   To stop backing /export/install/sparc/devlopment/modules with
   /export/install/sparc/modules, use

      backstop -d /export/install/sparc/devlopment/modules \
                  /export/install/sparc/modules

ENVIRONMENT VARIABLES
   PAGER   Page the internal manual page or license terms
           using "${PAGER}", otherwise print it.

EXIT STATUS
   An exit status of 0 is returned if successful, otherwise non-zero
   is returned.  The following error exit codes are used by
   backstop:

       EXIT CODE    MEANING
          251       Less than two operands.
          252       Not a directory, that is, trgdir or one or more
                    of the srcdir are not a directory.
          253       Not parallel directories, that is, a srcdir is
                    a sub-directory of trgdir or vice versa, or one
                    of the srcdir is a sub-directory of another
                    srcdir.
          254       Not yet implemented.
          255       Non-fatal errors occurred.  Error messages are
                    sent to STDERR for each non-fatal error that
                    occurred with return code equal to the number of
                    non-falal errors if less than 251, otherwise the
                    return code is 255, that is, "infinity".

FILES
   N/A.

SEE ALSO
   ln(1), mkdir(1), rm(1), rmdir(1), ksh(1), lndir(1), realpath(3)

   "Source Control to Project Control: Applying RCS and SCCS" by Don
   Bolinger and Tan Bronson. ISBN 1-56592-117-8.

NOTES
   Please send bug reports to https://github.com/mdgabriel/backstop/issues.

BUGS
   No known bugs to date.
```
