module Environment (Environment(..),defaultEnv) where

-- Section: environment (to be configured)
{- |
The environment contains the command name, operands, and the result of
every option at least.  It may contain other items that need to be
updated.
-}
data Environment =
    Env { command     :: String   -- ^ Program name
        , pwd         :: String   -- ^ Present Working Directory (PWD)
        , operands    :: [String] -- ^ @trgdir@ @srcdir@ @[srcdir...]@
        , absolute    :: Bool     -- ^ @-a@, use absolute symbolic links
        , deBackstop  :: Bool     -- ^ @-d@, de-backstop trgdir
        , help        :: Bool     -- ^ @-h@, display help summary
        , license     :: Bool     -- ^ @-l@, display license terms
        , manual      :: Bool     -- ^ @-m@, display internal man page
        , noAction    :: Bool     -- ^ @-n@, take no action but trace steps
        , populate    :: Bool     -- ^ @-p@, populate trgdir with sub-directories
        , report      :: Bool     -- ^ @-r@, report final statistics
        , trace       :: Bool     -- ^ @-t@, trace steps
        , version     :: Bool     -- ^ @-v@, display version
        , synopsis    :: Bool     -- ^ @-y@, display manual synopsis
        } deriving Show

-- The default Environment
{- |
The defualt values, otherwise updated from the flags supplied on by
the command line options.  The default program name is @backstop@.
All options flags are False or off by default.
-}
defaultEnv :: Environment
defaultEnv =
    Env { command     = "backstop" -- Default program name
        , pwd         = ""         -- No PWD by default
        , operands    = []         -- No operands by default
        , absolute    = False      -- False or off by default
        , deBackstop  = False      -- False or off by default
        , help        = False      -- False or off by default
        , license     = False      -- False or off by default
        , manual      = False      -- False or off by default
        , noAction    = False      -- False or off by default
        , populate    = False      -- False or off by default
        , report      = False      -- False or off by default
        , trace       = False      -- False or off by default
        , synopsis    = False      -- False or off by default
        , version     = False      -- False or off by default
        }
