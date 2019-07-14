module Main(main) where

import Control.Monad (unless)
import Data.List (sort,nub,intersect)
import System.Exit (exitWith,ExitCode(..))
import System.IO (hPutStrLn,hFlush,stdout,stderr)
import System.FilePath ((</>),pathSeparator,isAbsolute,isRelative,joinPath)
import System.Process (runCommand,waitForProcess)
import Test.HUnit hiding (Testable)
import Test.QuickCheck (Testable(..),Property(..),(==>),Result(..),quickCheckResult)
import Utils (putOrPageStrLn,anObject,aSymbolicLink,aDirectory,subdirectories
             ,subdirectory,root,reduceFilePath,compElems,pairs,unpairs,dot,dotdot
             ,relativeLink,absoluteLink)


-- Section 0 ------------------------------------------------------------------------

main :: IO ()
main = do putMessage "Unit test reduceFilePath:"
          runTestTT checks4reduceFilePath >>= checkCounts

          putMessage "Property test reduceFilePath:"
          mapM_ runTest props4reduceFilePath
          mapM_ runTest props4reduceFilePath_

          putMessage "Unit test absoluteLink:"
          runTestTT checks4absoluteLink >>= checkCounts

          putMessage "Property test absoluteLink:"
          mapM_ runTest props4absoluteLink

          putMessage "Unit test relativeLink:"
          runTestTT checks4relativeLink >>= checkCounts

          putMessage "Property test relativeLink:"
          mapM_ runTest props4relativeLink

          putMessage "Unit test pairs:"
          runTestTT checks4pairs >>= checkCounts

          putMessage "Unit test unpairs . pairs == id:"
          runTestTT checks4unpairsPairs >>= checkCounts

          putMessage "Property test unpairs . pairs == id:"
          mapM_ runTest props4unpairsPairs

          putMessage "Unit test compElems:"
          runTestTT check4compElems >>= checkCounts

          putMessage "Property test compElems:"
          mapM_ runTest props4check4compElems

          putMessage "Unit test subdirectory:"
          runTestTT checks4subdirectory >>= checkCounts

          putMessage "Unit test subdirectories:"
          runTestTT checks4subdirectories >>= checkCounts

          putMessage "Property test subdirectory and subdirectories:"
          mapM_ runTest props4Subdirectories

          putMessage "Unit test aDirectory:"
          runTestTT checks4aDirectory >>= checkCounts

          putMessage "Unit test aSymbolicLink:"
          runTestTT checks4aSymbolicLink >>= checkCounts

          putMessage "Unit test anObject:"
          runTestTT checks4anObject >>= checkCounts

          putMessage "Unit test putOrPageStrLn:"
          runTestTT checks4putOrPageStrLn >>= checkCounts

          putMessage "Running black box tests:"
          runTests >>= exitWith

checkCounts :: Counts -> IO ()
checkCounts cnts = let errs = errors cnts + failures cnts
                   in if errs > 0
                      then exitWith $ ExitFailure (if errs < 256 then errs else 255)
                      else putStrLn ""

runTest :: Testable t => (String, t) -> IO ()
runTest (s, t) = do putStrLn s
                    verify t
                    putStrLn ""

putMessage  :: String -> IO ()
putMessage s = let l = length s
               in do putStrLn s
                     (putStrLn . replicate l) '='
                     hFlush stdout


-- Section 1 ------------------------------------------------------------------------

checks4reduceFilePath :: Test
checks4reduceFilePath =
    let checkReduceFilePath p1 p2
            = TestCase (assertEqual "reduceFilePath" p2 (reduceFilePath p1))
    in TestList
           [
           -- The special case of null arguments
           checkReduceFilePath "" "."

           -- The special case of dot
           , checkReduceFilePath "." "."

           -- The special case of double dot
           , checkReduceFilePath ".." ".."

           -- Only slashes
           , checkReduceFilePath "/" "/"
           , checkReduceFilePath "//" "/"
           , checkReduceFilePath "///" "/"

           -- Only dots and slashes
           , checkReduceFilePath "./" "."
           , checkReduceFilePath ".//" "."
           , checkReduceFilePath ".///" "."
           , checkReduceFilePath "./." "."
           , checkReduceFilePath ".//." "."
           , checkReduceFilePath ".///." "."
           , checkReduceFilePath "././" "."
           , checkReduceFilePath ".//./././///." "."
           , checkReduceFilePath "././././././././" "."

           -- Only slashes and dots
           , checkReduceFilePath "/." "/"
           , checkReduceFilePath "//." "/"
           , checkReduceFilePath "/./" "/"
           , checkReduceFilePath "///.///" "/"
           , checkReduceFilePath "/./././." "/"
           , checkReduceFilePath "//.//.//.//." "/"

           -- Only slashes and double dots
           , checkReduceFilePath "/.." "/"
           , checkReduceFilePath "/../" "/"
           , checkReduceFilePath "///..///" "/"
           , checkReduceFilePath "/../../../../" "/"

           -- Only double dots and slashes
           , checkReduceFilePath "../" ".."
           , checkReduceFilePath "..///" ".."
           , checkReduceFilePath "..///.." "../.."
           , checkReduceFilePath "../../../../" "../../../.."
           , checkReduceFilePath "..///..///..///..///" "../../../.."

           -- Absolute paths with only slahses
           , checkReduceFilePath "/a" "/a"
           , checkReduceFilePath "/a/b" "/a/b"
           , checkReduceFilePath "//a" "/a"
           , checkReduceFilePath "//a//b" "/a/b"
           , checkReduceFilePath "//a///" "/a"
           , checkReduceFilePath "//a//b///" "/a/b"
                                 
           -- Relative paths with only slahses
           , checkReduceFilePath "a" "a"
           , checkReduceFilePath "a/b" "a/b"
           , checkReduceFilePath "a" "a"
           , checkReduceFilePath "a//b" "a/b"
           , checkReduceFilePath "a///" "a"
           , checkReduceFilePath "a//b///" "a/b"

           -- Absolute paths with dots
           , checkReduceFilePath "/a/." "/a"
           , checkReduceFilePath "/a/./." "/a"
           , checkReduceFilePath "/./a" "/a"
           , checkReduceFilePath "/./a/./." "/a"

           -- Relative paths with dots
           , checkReduceFilePath "a/." "a"
           , checkReduceFilePath "a/./." "a"
           , checkReduceFilePath "./a" "a"
           , checkReduceFilePath "./a/./." "a"

           -- Absolute paths with double dots
           , checkReduceFilePath "/.." "/"
           , checkReduceFilePath "/a/.." "/"
           , checkReduceFilePath "/a/b/.." "/a"
           , checkReduceFilePath "/a/b/c/.." "/a/b"
           , checkReduceFilePath "/a/../b" "/b"
           , checkReduceFilePath "/a/b/../c" "/a/c"
           , checkReduceFilePath "/a/b/c/../.." "/a"
           , checkReduceFilePath "/a/b/c/../../.." "/"
           , checkReduceFilePath "/a/b/c/../../../.." "/"
           , checkReduceFilePath "/a/b/c/../x/y/../z" "/a/b/x/z"
                                 
           -- Relative paths with double dots
           , checkReduceFilePath "a/.." "."
           , checkReduceFilePath "a/b/.." "a"
           , checkReduceFilePath "a/b/c/.." "a/b"
           , checkReduceFilePath "a/../b" "b"
           , checkReduceFilePath "a/b/../c" "a/c"
           , checkReduceFilePath "a/b/c/../.." "a"
           , checkReduceFilePath "a/b/c/../../.." "."
           , checkReduceFilePath "a/b/c/../../../.." ".."
           , checkReduceFilePath "a/b/c/../../../../.." "../.."
           , checkReduceFilePath "a/b/c/../x/y/../z" "a/b/x/z"

           -- General abosulte paths
           , checkReduceFilePath "/a/b/./x/y/z/../." "/a/b/x/y"
           , checkReduceFilePath "/./a/.." "/"
           , checkReduceFilePath "/a/./b/c/../x/y/./.." "/a/b/x"
           , checkReduceFilePath "/a/b/c/d/e/f/g/h/../../../../../e/f/g/h/i" "/a/b/c/e/f/g/h/i"

           -- General reltative paths
           , checkReduceFilePath "a/b/./x/y/z/../." "a/b/x/y"
           , checkReduceFilePath "./a/.." "."
           , checkReduceFilePath "a/./b/c/../x/y/./.." "a/b/x"
           , checkReduceFilePath "./../../../a/b/c/./x/y/z" "../../../a/b/c/x/y/z"
           , checkReduceFilePath "a/b/c/d/e/f/g/h/../../../../../e/f/g/h/i" "a/b/c/e/f/g/h/i"
           ]


-- Section 2 ------------------------------------------------------------------------

props4reduceFilePath :: [(String, FilePath -> Property)]
props4reduceFilePath = [ ("propReduceFilePathIdempotent", propReduceFilePathIdempotent)
                       , ("propReduceFilePathStaysAbsolute", propReduceFilePathStaysAbsolute)
                       , ("propReduceFilePathStaysRelative", propReduceFilePathStaysRelative)
                       ]

props4reduceFilePath_ :: [(String, FilePath -> FilePath -> Property)]
props4reduceFilePath_ = [ ("propReduceFilePathAssociativity", propReduceFilePathAssociativity)
                        ]

propReduceFilePathIdempotent  :: FilePath -> Property
propReduceFilePathIdempotent p =
    property $ (reduceFilePath . reduceFilePath) p == reduceFilePath p

propReduceFilePathStaysAbsolute   :: FilePath -> Property
propReduceFilePathStaysAbsolute p =
    let p' = root </> p
    in property $ (isAbsolute . reduceFilePath) p' == isAbsolute p'

propReduceFilePathStaysRelative   :: FilePath -> Property
propReduceFilePathStaysRelative p =
    (not (null p) && (head p /= pathSeparator)) ==>
        (isRelative . reduceFilePath) p == isRelative p

propReduceFilePathAssociativity :: FilePath -> FilePath -> Property
propReduceFilePathAssociativity p1 p2 =
    property (reduceFilePath (p1 </> p2) == reduceFilePath (reduceFilePath p1 </> reduceFilePath p2))


-- Section 3 ------------------------------------------------------------------------

checks4absoluteLink :: Test
checks4absoluteLink =
    let
        checkAbsoluteLink wd p1 p2 p =
            TestCase (assertEqual "absoluteLink" p (absoluteLink wd p1 p2))
    in TestList
           [ checkAbsoluteLink ""          ""  ""            root
           , checkAbsoluteLink ""          ""  "."           root
           , checkAbsoluteLink ""          ""  ".."          root
           , checkAbsoluteLink ""          "X" ""            root
           , checkAbsoluteLink ""          "X" "."           root
           , checkAbsoluteLink ""          "X" ".."          root
           , checkAbsoluteLink root        ""  ""            root
           , checkAbsoluteLink root        ""  "."           root
           , checkAbsoluteLink root        ""  ".."          root
           , checkAbsoluteLink root        "X" ""            root
           , checkAbsoluteLink root        "X" "."           root
           , checkAbsoluteLink root        "X" ".."          root
           , checkAbsoluteLink "/xx/yy/zz" ""  ""            "/xx/yy/zz"
           , checkAbsoluteLink "/xx/yy/zz" ""  "."           "/xx/yy/zz"
           , checkAbsoluteLink "/xx/yy/zz" ""  ".."          "/xx/yy"
           , checkAbsoluteLink "/xx/yy/zz" "X" ""            "/xx/yy/zz"
           , checkAbsoluteLink "/xx/yy/zz" "X" "."           "/xx/yy/zz"
           , checkAbsoluteLink "/xx/yy/zz" "X" ".."          "/xx/yy"
           , checkAbsoluteLink "xx/yy/zz"  ""  ""            "/xx/yy/zz"
           , checkAbsoluteLink "xx/yy/zz"  ""  "."           "/xx/yy/zz"
           , checkAbsoluteLink "xx/yy/zz"  ""  ".."          "/xx/yy"
           , checkAbsoluteLink "xx/yy/zz"  "X" ""            "/xx/yy/zz"
           , checkAbsoluteLink "xx/yy/zz"  "X" "."           "/xx/yy/zz"
           , checkAbsoluteLink "xx/yy/zz"  "X" ".."          "/xx/yy"
           , checkAbsoluteLink ""          ""  "aa/bb/cc"    "/aa/bb/cc"
           , checkAbsoluteLink ""          ""  "aa/bb/cc/."  "/aa/bb/cc"
           , checkAbsoluteLink ""          ""  "aa/bb/cc/.." "/aa/bb"
           , checkAbsoluteLink ""          "X" "aa/bb/cc/"   "/aa/bb/cc"
           , checkAbsoluteLink ""          "X" "aa/bb/cc/."  "/aa/bb/cc"
           , checkAbsoluteLink ""          "X" "aa/bb/cc/.." "/aa/bb"
           , checkAbsoluteLink root        ""  "aa/bb/cc/"   "/aa/bb/cc"
           , checkAbsoluteLink root        ""  "aa/bb/cc/."  "/aa/bb/cc"
           , checkAbsoluteLink root        ""  "aa/bb/cc/.." "/aa/bb"
           , checkAbsoluteLink root        "X" "aa/bb/cc/"   "/aa/bb/cc"
           , checkAbsoluteLink root        "X" "aa/bb/cc/."  "/aa/bb/cc"
           , checkAbsoluteLink root        "X" "aa/bb/cc/.." "/aa/bb"
           , checkAbsoluteLink "/xx/yy/zz" ""  "aa/bb/cc/"   "/xx/yy/zz/aa/bb/cc"
           , checkAbsoluteLink "/xx/yy/zz" ""  "aa/bb/cc/."  "/xx/yy/zz/aa/bb/cc"
           , checkAbsoluteLink "/xx/yy/zz" ""  "aa/bb/cc/.." "/xx/yy/zz/aa/bb"
           , checkAbsoluteLink "/xx/yy/zz" "X" "aa/bb/cc/"   "/xx/yy/zz/aa/bb/cc"
           , checkAbsoluteLink "/xx/yy/zz" "X" "aa/bb/cc/."  "/xx/yy/zz/aa/bb/cc"
           , checkAbsoluteLink "/xx/yy/zz" "X" "aa/bb/cc/.." "/xx/yy/zz/aa/bb"
           , checkAbsoluteLink "xx/yy/zz"  ""  "aa/bb/cc/"   "/xx/yy/zz/aa/bb/cc"
           , checkAbsoluteLink "xx/yy/zz"  ""  "aa/bb/cc/."  "/xx/yy/zz/aa/bb/cc"
           , checkAbsoluteLink "xx/yy/zz"  ""  "aa/bb/cc/.." "/xx/yy/zz/aa/bb"
           , checkAbsoluteLink "xx/yy/zz"  "X" "aa/bb/cc/"   "/xx/yy/zz/aa/bb/cc"
           , checkAbsoluteLink "xx/yy/zz"  "X" "aa/bb/cc/."  "/xx/yy/zz/aa/bb/cc"
           , checkAbsoluteLink "xx/yy/zz"  "X" "aa/bb/cc/.." "/xx/yy/zz/aa/bb"
           ]


-- Section 4 ------------------------------------------------------------------------

wd :: FilePath
wd  = joinPath $ map show [0..9]

props4absoluteLink :: [(String, FilePath -> FilePath -> Property)]
props4absoluteLink = [ ("propAbsoluteLinkIdempotent root", propAbsoluteLinkIdempotent root)
                     , ("propAbsoluteLinkIdempotent wd", propAbsoluteLinkIdempotent wd)
                     , ("propAbsoluteLinkStaysAbsolute root", propAbsoluteLinkStaysAbsolute root)
                     , ("propAbsoluteLinkStaysAbsolute wd", propAbsoluteLinkStaysAbsolute wd)
                     , ("propAbsoluteLinkIsNotRelative root", propAbsoluteLinkIsNotRelative root)
                     , ("propAbsoluteLinkIsNotRelative wd", propAbsoluteLinkIsNotRelative wd)
                     ]

propAbsoluteLinkIdempotent  :: FilePath -> FilePath -> FilePath -> Property
propAbsoluteLinkIdempotent wd p1 p2 =
    property $ (absoluteLink wd p1 . absoluteLink wd p1) p2 == absoluteLink wd p1 p2

propAbsoluteLinkStaysAbsolute   :: FilePath -> FilePath -> FilePath -> Property
propAbsoluteLinkStaysAbsolute wd p1 =
    property . isAbsolute . absoluteLink wd p1

propAbsoluteLinkIsNotRelative   :: FilePath -> FilePath -> FilePath -> Property
propAbsoluteLinkIsNotRelative wd p1 =
    property . not. isRelative . absoluteLink wd p1


-- Section 5 ------------------------------------------------------------------------

checks4relativeLink :: Test
checks4relativeLink =
    let
        checkRelativeLink wd p1 p2 p =
            TestCase (assertEqual "relativeLink" p (relativeLink wd p1 p2))
    in TestList
           [ checkRelativeLink ""          ""  ""            dot
           , checkRelativeLink ""          ""  "."           dot
           , checkRelativeLink ""          ""  ".."          dot
           , checkRelativeLink ""          "X" ""            dot
           , checkRelativeLink ""          "X" "."           dot
           , checkRelativeLink ""          "X" ".."          dot
           , checkRelativeLink root        ""  ""            dot
           , checkRelativeLink root        ""  "."           dot
           , checkRelativeLink root        ""  ".."          dot
           , checkRelativeLink root        "X" ""            dot
           , checkRelativeLink root        "X" "."           dot
           , checkRelativeLink root        "X" ".."          dot
           , checkRelativeLink "/xx/yy/zz" ""  ""            dot
           , checkRelativeLink "/xx/yy/zz" ""  "."           dot
           , checkRelativeLink "/xx/yy/zz" ""  ".."          dot
           , checkRelativeLink "/xx/yy/zz" "X" ""            dot
           , checkRelativeLink "/xx/yy/zz" "X" "."           dot
           , checkRelativeLink "/xx/yy/zz" "X" ".."          dotdot
           , checkRelativeLink "xx/yy/zz"  ""  ""            dot
           , checkRelativeLink "xx/yy/zz"  ""  "."           dot
           , checkRelativeLink "xx/yy/zz"  ""  ".."          dot
           , checkRelativeLink "xx/yy/zz"  "X" ""            dot
           , checkRelativeLink "xx/yy/zz"  "X" "."           dot
           , checkRelativeLink "xx/yy/zz"  "X" ".."          dotdot
           , checkRelativeLink ""          ""  "aa/bb/cc"    "aa/bb/cc"
           , checkRelativeLink ""          ""  "aa/bb/cc/."  "aa/bb/cc"
           , checkRelativeLink ""          ""  "aa/bb/cc/.." "aa/bb"
           , checkRelativeLink ""          "X" "aa/bb/cc/"   "aa/bb/cc"
           , checkRelativeLink ""          "X" "aa/bb/cc/."  "aa/bb/cc"
           , checkRelativeLink ""          "X" "aa/bb/cc/.." "aa/bb"
           , checkRelativeLink root        ""  "aa/bb/cc/"   "aa/bb/cc"
           , checkRelativeLink root        ""  "aa/bb/cc/."  "aa/bb/cc"
           , checkRelativeLink root        ""  "aa/bb/cc/.." "aa/bb"
           , checkRelativeLink root        "X" "aa/bb/cc/"   "aa/bb/cc"
           , checkRelativeLink root        "X" "aa/bb/cc/."  "aa/bb/cc"
           , checkRelativeLink root        "X" "aa/bb/cc/.." "aa/bb"
           , checkRelativeLink "/xx/yy/zz" ""  "aa/bb/cc/"   "aa/bb/cc"
           , checkRelativeLink "/xx/yy/zz" ""  "aa/bb/cc/."  "aa/bb/cc"
           , checkRelativeLink "/xx/yy/zz" ""  "aa/bb/cc/.." "aa/bb"
           , checkRelativeLink "/xx/yy/zz" "X" "aa/bb/cc/"   "aa/bb/cc"
           , checkRelativeLink "/xx/yy/zz" "X" "aa/bb/cc/."  "aa/bb/cc"
           , checkRelativeLink "/xx/yy/zz" "X" "aa/bb/cc/.." "aa/bb"
           , checkRelativeLink "xx/yy/zz"  ""  "aa/bb/cc/"   "aa/bb/cc"
           , checkRelativeLink "xx/yy/zz"  ""  "aa/bb/cc/."  "aa/bb/cc"
           , checkRelativeLink "xx/yy/zz"  ""  "aa/bb/cc/.." "aa/bb"
           , checkRelativeLink "xx/yy/zz"  "X" "aa/bb/cc/"   "aa/bb/cc"
           , checkRelativeLink "xx/yy/zz"  "X" "aa/bb/cc/."  "aa/bb/cc"
           , checkRelativeLink "xx/yy/zz"  "X" "aa/bb/cc/.." "aa/bb"
           , checkRelativeLink "xx/yy/zz"  "a" "a/bb/cc"     "bb/cc"
           , checkRelativeLink "xx/yy"   "a/b" "a/b/cc"      "cc"
           ]


-- Section 6 ------------------------------------------------------------------------

props4relativeLink :: [(String, FilePath -> FilePath -> Property)]
props4relativeLink = [ ("propRelativeLinkStaysRelative root", propRelativeLinkStaysRelative root)
                     , ("propRelativeLinkStaysRelative wd", propRelativeLinkStaysRelative wd)
                     , ("propRelativeLinkIsNotAbsolute root", propRelativeLinkIsNotAbsolute root)
                     , ("propRelativeLinkIsNotAbsolute wd", propRelativeLinkIsNotAbsolute wd)
                     , ("propSubRelativeLinkIsTarget root", propSubRelativeLinkIsTarget root)
                     , ("propSubRelativeLinkIsTarget wd", propSubRelativeLinkIsTarget wd)
                     ]

propRelativeLinkStaysRelative :: FilePath -> FilePath -> FilePath -> Property
propRelativeLinkStaysRelative wd p1 = property . isRelative . relativeLink wd p1

propRelativeLinkIsNotAbsolute :: FilePath -> FilePath -> FilePath -> Property
propRelativeLinkIsNotAbsolute wd p1 = property . not. isAbsolute . relativeLink wd p1

propSubRelativeLinkIsTarget :: FilePath -> FilePath -> FilePath -> Property
propSubRelativeLinkIsTarget wd p1 p2 =
    let rl    = relativeLink wd (p1 </> "symlink") p2
        abs p = if isAbsolute p then p else (pathSeparator:wd) </> p
    in property (reduceFilePath (abs p2) == reduceFilePath (abs p1 </> rl))


-- Section 7 ------------------------------------------------------------------------

checks4pairs :: Test
checks4pairs =
    let checkPairs xs ps = TestCase (assertEqual "pairs" ps (pairs xs))
    in TestList
           [
           -- Null and one element
             checkPairs []  ([] :: [(Int,Int)])
           , checkPairs [0]  []

           -- One pair
           , checkPairs [0,0] [(0,0)]
           , checkPairs [0,1] [(0,1)]
           , checkPairs [1,0] [(1,0)]

           -- Three pairs
           , checkPairs [0,0,0] [(0,0),(0,0),(0,0)]
           , checkPairs [0,0,2] [(0,0),(0,2),(0,2)]
           , checkPairs [0,1,0] [(0,1),(0,0),(1,0)]
           , checkPairs [0,1,1] [(0,1),(0,1),(1,1)]
           , checkPairs [0,1,2] [(0,1),(0,2),(1,2)]
           , checkPairs [1,0,2] [(1,0),(1,2),(0,2)]
           , checkPairs [2,1,0] [(2,1),(2,0),(1,0)]
           , checkPairs [0,2,1] [(0,2),(0,1),(2,1)]
           ]


-- Section 8 ------------------------------------------------------------------------

checks4unpairsPairs :: Test
checks4unpairsPairs =
    let l = [[],[0,0],[0,1],[1,0],[0,0,0],[0,0,2],[0,1,0],[0,1,1],[0,1,2],[1,0,2],[2,1,0],[0,2,1]]
    in TestCase (assertEqual "unpairs/pairs" l (map (unpairs.pairs) l))

props4unpairsPairs :: [(String, [Int] -> Property)]
props4unpairsPairs = [("propUnpairsPairsId", propUnpairsPairsId)]

propUnpairsPairsId   :: [Int] -> Property
propUnpairsPairsId xs = length xs /= 1 ==> xs == (unpairs.pairs) xs


-- Section 9 ------------------------------------------------------------------------

check4compElems :: Test
check4compElems =
    let checkCompElems xs ys ts = TestCase (assertEqual "compElems" ts (compElems xs ys))
    in TestList
           [ checkCompElems ([]::[Int]) []       ([],[],[])
           , checkCompElems [0..9]      []       ([],[0..9],[])
           , checkCompElems []          [0..9]   ([],[],[0..9])
           , checkCompElems [0..9]      [0..9]   ([0..9],[],[])
           , checkCompElems [0..9]    [10..19]   ([],[0..9],[10..19])
           , checkCompElems [0..9]     [0..19]   ([0..9],[],[10..19])
           , checkCompElems [0..19]     [0..9]   ([0..9],[10..19],[])
           , checkCompElems [0..19]    [-9..9]   ([0..9],[10..19],[-9..(-1)])
           ]

props4check4compElems :: [(String,[Int] -> [Int] -> Property)]
props4check4compElems = [("propCompElemsDisjoint",propCompElemsDisjoint)
                        ,("propCompElemsComplete",propCompElemsComplete)
                        ]

propCompElemsDisjoint :: [Int] -> [Int] -> Property
propCompElemsDisjoint xs ys = let (same, unique1, unique2) = compElems xs ys
                              in property (
                                           null (same `intersect` unique1) &&
                                           null (same `intersect` unique2) &&
                                           null (unique1 `intersect` unique2)
                                          )

propCompElemsComplete :: [Int] -> [Int] -> Property
propCompElemsComplete xs ys = let (same, unique1, unique2) = compElems xs ys
                              in property (
                                           sort (nub xs) == sort (same++unique1) &&
                                           sort (nub ys) == sort (same++unique2)
                                          )


-- Section 10 -----------------------------------------------------------------------

checks4subdirectory :: Test
checks4subdirectory =
    let checkSubdirectory d1 d2 b = TestCase (assertEqual "subdirectory" b (d1 `subdirectory` d2))
    in TestList
           [ checkSubdirectory "/"      "/"      True
           , checkSubdirectory "/"      "/."     True
           , checkSubdirectory "/."     "/"      True
           , checkSubdirectory "/."     "/."     True
           , checkSubdirectory "/"      "/.."    True
           , checkSubdirectory "/.."    "/"      True
           , checkSubdirectory "/.."    "/.."    True
           , checkSubdirectory "/.."    "/."     True
           , checkSubdirectory "/."     "/.."    True
           , checkSubdirectory "/a/b/c" "/x/y/z" False
           , checkSubdirectory "/x/y/z" "/a/b/c" False
           , checkSubdirectory "/x/y/z" "/x/y/z" True
           , checkSubdirectory "/x/y/z" "/x/y"   True
           , checkSubdirectory "/x/y"   "/x/y/z" False
           ]

checks4subdirectories :: Test
checks4subdirectories =
    let checkSubdirectories d2 d1 b = TestCase (assertEqual "subdirectories" b (d2 `subdirectories` d1))
    in TestList
           [ checkSubdirectories "/"      "/"      True
           , checkSubdirectories "/"      "/."     True
           , checkSubdirectories "/."     "/"      True
           , checkSubdirectories "/."     "/."     True
           , checkSubdirectories "/"      "/.."    True
           , checkSubdirectories "/.."    "/"      True
           , checkSubdirectories "/.."    "/.."    True
           , checkSubdirectories "/.."    "/."     True
           , checkSubdirectories "/."     "/.."    True
           , checkSubdirectories "/a/b/c" "/x/y/z" False
           , checkSubdirectories "/x/y/z" "/a/b/c" False
           , checkSubdirectories "/x/y/z" "/x/y/z" True
           , checkSubdirectories "/x/y/z" "/x/y"   True
           , checkSubdirectories "/x/y"   "/x/y/z" True
           ]

props4Subdirectories :: [(String, FilePath -> FilePath -> Property)]
props4Subdirectories = [ ("propSubdirectory",propSubdirectory)
                       , ("propSubdirectories",propSubdirectories)
                       ]

propSubdirectory :: FilePath -> FilePath -> Property
propSubdirectory d1 d2 =
    let d1_ = root </> d1
        d2_ = root </> d2
    in property (
                 if reduceFilePath d1_ == reduceFilePath d2_
                 then d1_ `subdirectory` d2_
                 else if d1_ `subdirectory` d2_
                      then not (d2_ `subdirectory` d1_)
                      else if d2_ `subdirectory` d1_
                           then not (d1_ `subdirectory` d2_)
                           else not (d1_ `subdirectory` d2_) && not (d2_ `subdirectory` d1_)
                )

propSubdirectories :: FilePath -> FilePath -> Property
propSubdirectories d1 d2 =
    let d1_ = root </> d1
        d2_ = root </> d2
    in property (
                 (subdirectories d1_ d2_ && subdirectories d2_ d1_) ||
                 (not (subdirectories d1_ d2_) && not (subdirectories d2_ d1_))
                )


-- Section 11 -----------------------------------------------------------------------

checks4aDirectory :: Test
checks4aDirectory =
    let check4aDirectory p expected = 
            TestCase (aDirectory p >>= assertBool "aDirectory" . (expected==))
    in TestList
           [ check4aDirectory "/"           True
           , check4aDirectory "."           True
           , check4aDirectory ".."          True
           , check4aDirectory "/etc/passwd" False
           , check4aDirectory "/var/run"    False
           , check4aDirectory "RaNdOmJuNk"  False
           ]

checks4aSymbolicLink :: Test
checks4aSymbolicLink =
    let check4aSymbolicLink p expected = 
            TestCase (aSymbolicLink p >>= assertBool "aSymbolicLink" . (expected==))
    in TestList
           [ check4aSymbolicLink "/"           False
           , check4aSymbolicLink "."           False
           , check4aSymbolicLink ".."          False
           , check4aSymbolicLink "/etc/passwd" False
           , check4aSymbolicLink "/var/run"    True
           , check4aSymbolicLink "RaNdOmJuNk"  False
           ]

checks4anObject :: Test
checks4anObject =
    let check4anObject p expected = 
            TestCase (anObject p >>= assertBool "anObject" . (expected==))
    in TestList
           [ check4anObject "/"           True
           , check4anObject "."           True
           , check4anObject ".."          True
           , check4anObject "/etc/passwd" True
           , check4anObject "RaNdOmJuNk"  False
           , check4anObject "/cdrom"      True
           ]


-- Section 12 -----------------------------------------------------------------------

checks4putOrPageStrLn :: Test
checks4putOrPageStrLn =
    let check4putOrPageStrLn s =
            TestCase $ assert (putOrPageStrLn s >>= \e -> unless (e == ExitSuccess) $ exitWith e)
    in TestList
           [ check4putOrPageStrLn ""
           , check4putOrPageStrLn "."
           ]


-- Section 13 ------------------------------------------------------------------------

runTests :: IO ExitCode
runTests =
    runCommand "./tests/run-tests.sh dist*/build/*/ghc-*/backstop-*/x/backstop/build/backstop/backstop"
                   >>= waitForProcess

-- Section Last ---------------------------------------------------------------------

verify :: Testable a => a -> IO ()
verify  t =
    do result <- quickCheckResult t
       case result of
         Success{}                              -> return ()
         GaveUp  ntest _ _ _ _ _                -> do hPutStrLn stderr ("Gave up after "++show ntest++" tests")
                                                      exitWith (ExitFailure (if ntest < 255 then ntest else 255))
         Failure _ _ _ _ _ _ _ reason _ _ _ _ _ -> do hPutStrLn stderr ("Failure: "++reason)
                                                      exitWith (ExitFailure 255)
         _                                      -> do hPutStrLn stderr "Not a success"
                                                      exitWith (ExitFailure 255)
