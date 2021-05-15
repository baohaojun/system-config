-- Copyright (C) 2016-2019 Sergey Vinokurov <serg.foo@gmail.com>
-- Copyright (C) 2014-2016 Sebastian Wiesner <swiesner@lunaryorn.com>
-- Copyright (C) 2016-2018 Danny Navarro <j@dannynavarro.net>
-- Copyright (C) 2015 Mark Karpov <markkarpov@opmbx.org>
-- Copyright (C) 2015 Michael Alan Dorman <mdorman@ironicdesign.com>
-- Copyright (C) 2014 Gracjan Polak <gracjanpolak@gmail.com>

-- This file is not part of GNU Emacs.

-- This program is free software; you can redistribute it and/or modify it under
-- the terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.

-- This program is distributed in the hope that it will be useful, but WITHOUT
-- ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
-- FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
-- details.

-- You should have received a copy of the GNU General Public License along with
-- this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE BangPatterns         #-}
{-# LANGUAGE CPP                  #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}

module Main (main) where

#if __GLASGOW_HASKELL__ >= 800
#define GHC_INCLUDES_VERSION_MACRO 1
#endif

#if defined(GHC_INCLUDES_VERSION_MACRO)
# if MIN_VERSION_Cabal(3, 2, 0)
#  define Cabal32 1
#  define Cabal32OrLater 1
#  define Cabal30OrLater 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(3, 0, 0)
#  define Cabal30 1
#  define Cabal30OrLater 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(2, 3, 0)
#  define Cabal24 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(2, 1, 0)
#  define Cabal22 1
#  define Cabal22OrLater 1
#  define Cabal20OrLater 1
# elif MIN_VERSION_Cabal(2, 0, 0)
#  define Cabal20 1
#  define Cabal20OrLater 1
# endif
#else
-- Hack - we may actually be using Cabal 2.0 with e.g. 7.8 GHC. But
-- that's not likely to occur for average user who's relying on
-- packages bundled with GHC. The 2.0 Cabal is bundled starting with 8.2.1.
# undef Cabal24
# undef Cabal22
# undef Cabal20

#endif

#if __GLASGOW_HASKELL__ >= 704
# define Cabal114OrMore 1
#endif

#if __GLASGOW_HASKELL__ < 710
# define Cabal118OrLess 1
#endif

#if __GLASGOW_HASKELL__ <= 763
#undef HAVE_DATA_FUNCTOR_IDENTITY
#else
#define HAVE_DATA_FUNCTOR_IDENTITY
#endif

import qualified Data.ByteString.Char8 as C8
#if __GLASGOW_HASKELL__ > 704
import qualified Data.ByteString.Lazy.Builder as CL8.Builder
#else
import qualified Data.ByteString.Lazy.Char8 as CL8
#endif

import Distribution.ModuleName (ModuleName)
import qualified Distribution.ModuleName as ModuleName
import Distribution.PackageDescription ()

#if defined(Cabal20OrLater)
import Distribution.Types.Benchmark (benchmarkBuildInfo, benchmarkInterface)
import Distribution.Types.ForeignLib (foreignLibBuildInfo)
import Distribution.Types.TestSuite (testBuildInfo, testInterface)

import Distribution.PackageDescription (allLibraries, libName)
#else
import Distribution.PackageDescription (library)
#endif


import qualified Control.Applicative as A
import Control.Exception (SomeException, try)
import Control.Monad (when)
#if defined(Cabal22OrLater)
import qualified Data.ByteString as BS
#endif
import Data.Char (isSpace)
#if defined(HAVE_DATA_FUNCTOR_IDENTITY)
import Data.Functor.Identity
#endif
import Data.List (nub, foldl', intersperse)
import Data.Maybe (maybeToList)
#if __GLASGOW_HASKELL__ < 710
import Data.Monoid
#endif
#if defined(Cabal32OrLater)
import Data.List.NonEmpty (toList)
#endif
import Data.Set (Set)
import qualified Data.Set as S
#ifdef Cabal118OrLess
import Distribution.Compiler
       (CompilerFlavor(GHC), CompilerId(CompilerId), buildCompilerFlavor)
#else
import Distribution.Compiler
       (AbiTag(NoAbiTag), CompilerFlavor(GHC), CompilerId(CompilerId),
        CompilerInfo, buildCompilerFlavor, unknownCompilerInfo)
#endif
import Distribution.Package
       (pkgName, Dependency(..))
import Distribution.PackageDescription
       (GenericPackageDescription, PackageDescription(..),
        TestSuiteInterface(..), BuildInfo(..), Library, Executable,
        allBuildInfo, usedExtensions, allLanguages, hcOptions, exeName,
        buildInfo, modulePath, libBuildInfo, exposedModules)
import Distribution.Simple.BuildPaths (defaultDistPref)
import Distribution.Simple.Utils (cabalVersion)
import Distribution.System (buildPlatform)
#if defined(Cabal20OrLater)
import Distribution.System (OS(OSX), buildArch, buildOS)
#endif
import Distribution.Text (display)
#if defined(Cabal20OrLater)
import Distribution.Types.PackageId (PackageId)
#endif
import Distribution.Verbosity (silent)
import Language.Haskell.Extension (Extension(..),Language(..))
import System.Console.GetOpt
import System.Environment (getArgs)
import System.Exit (ExitCode(..), exitFailure, exitSuccess)
import System.FilePath ((</>), dropFileName, normalise, isPathSeparator)
import System.Info (compilerVersion)
import System.IO (Handle, hGetContents, hPutStrLn, stderr, stdout)
import System.Process (readProcessWithExitCode)
import qualified System.Process as Process

#if __GLASGOW_HASKELL__ >= 710 && !defined(Cabal20) && !defined(Cabal22OrLater)
import Data.Version (Version)
#endif

#if defined(Cabal24) || defined(Cabal30) || defined(Cabal32)
import Distribution.PackageDescription (allBuildDepends)
#endif

#if defined(Cabal114OrMore)
import Distribution.PackageDescription (BenchmarkInterface(..),)
#endif

#if defined(Cabal20OrLater)
import Control.Monad (filterM)
import Distribution.Package (unPackageName, depPkgName, PackageName)
import Distribution.PackageDescription.Configuration (finalizePD)
import Distribution.Types.ComponentRequestedSpec (ComponentRequestedSpec(..))
import Distribution.Types.ForeignLib (ForeignLib(foreignLibName))
import Distribution.Types.UnqualComponentName (unUnqualComponentName)
import qualified Distribution.Version as CabalVersion
import Distribution.Types.Benchmark (Benchmark(benchmarkName))
import Distribution.Types.TestSuite (TestSuite(testName))
import System.Directory (doesDirectoryExist, doesFileExist)
#else
import Control.Arrow (second)
import Data.Version (showVersion)
import Distribution.Package (PackageName(..))
import Distribution.PackageDescription.Configuration
       (finalizePackageDescription, mapTreeData)

# if defined(Cabal114OrMore)
import Distribution.PackageDescription
       (TestSuite(..), Benchmark(..),
        condTestSuites, condBenchmarks, benchmarkEnabled, testEnabled)
# else
import Distribution.PackageDescription
       (TestSuite(..), condTestSuites, testEnabled)
# endif
#endif

#if defined(Cabal22OrLater)
import Distribution.Pretty (prettyShow)
#endif

#if defined(Cabal32OrLater)
import Distribution.PackageDescription (mkFlagAssignment)
#elif defined(Cabal22OrLater)
import Distribution.Types.GenericPackageDescription (mkFlagAssignment)
#endif

#if defined(Cabal22OrLater)
import Distribution.PackageDescription.Parsec
       (runParseResult, readGenericPackageDescription, parseGenericPackageDescription)
# if defined(Cabal30OrLater)
import Distribution.Parsec.Error (showPError)
#else
import Distribution.Parsec.Common (showPError)
# endif
#elif defined(Cabal20)
import Distribution.PackageDescription.Parse
       (ParseResult(..), readGenericPackageDescription, parseGenericPackageDescription)
import Distribution.ParseUtils (locatedErrorMsg)
#else
import Distribution.PackageDescription.Parse
       (ParseResult(..), parsePackageDescription, readPackageDescription)
import Distribution.ParseUtils (locatedErrorMsg)
#endif

#if defined(Cabal30OrLater)
import Distribution.Types.LibraryName (libraryNameString)
#endif

newtype UnixFilepath = UnixFilepath { unUnixFilepath :: C8.ByteString }
  deriving (Eq, Ord)

mkUnixFilepath :: FilePath -> UnixFilepath
mkUnixFilepath = UnixFilepath . C8.map (\c -> if isPathSeparator c then '/' else c) . C8.pack . normalise

data Sexp
    = SList [Sexp]
    | SString C8.ByteString
    | SSymbol C8.ByteString

data TargetTool = Cabal | Stack
#if defined(Cabal20OrLater)
                        | CabalNew PackageId GhcVersion

type GhcVersion = String
#endif

#if __GLASGOW_HASKELL__ > 704
type Builder = CL8.Builder.Builder

builderFromByteString :: C8.ByteString -> Builder
builderFromByteString = CL8.Builder.byteString

builderFromChar :: Char -> Builder
builderFromChar = CL8.Builder.char8

hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder = CL8.Builder.hPutBuilder

#else
type Builder = Endo CL8.ByteString

builderFromByteString :: C8.ByteString -> Builder
builderFromByteString x = Endo (CL8.fromChunks [x] `mappend`)

builderFromChar :: Char -> Builder
builderFromChar c = Endo (CL8.singleton c `mappend`)

hPutBuilder :: Handle -> Builder -> IO ()
hPutBuilder h (Endo f) = CL8.hPut h $ f CL8.empty
#endif

sym :: C8.ByteString -> Sexp
sym = SSymbol

renderSexp :: Sexp -> Builder
renderSexp (SSymbol s) = builderFromByteString s
renderSexp (SString s) = dquote `mappend` builderFromByteString s `mappend` dquote
  where
    dquote = builderFromChar '"'
renderSexp (SList xs)  =
    lparen `mappend` mconcat (intersperse space (map renderSexp xs)) `mappend` rparen
  where
    lparen = builderFromChar '('
    rparen = builderFromChar ')'
    space  = builderFromChar ' '

class ToSexp a  where
    toSexp :: a -> Sexp

instance ToSexp C8.ByteString where
    toSexp = SString

instance ToSexp UnixFilepath where
    toSexp = SString . unUnixFilepath

instance ToSexp Extension where
    toSexp (EnableExtension ext) = toSexp (C8.pack (show ext))
    toSexp (DisableExtension ext) = toSexp ("No" `mappend` C8.pack (show ext))
    toSexp (UnknownExtension ext) = toSexp (C8.pack ext)

instance ToSexp Language where
    toSexp (UnknownLanguage lang) = toSexp (C8.pack lang)
    toSexp lang = toSexp (C8.pack (show lang))

instance ToSexp Dependency where
    toSexp = toSexp . C8.pack . unPackageName' . depPkgName'

instance ToSexp Sexp where
    toSexp = id

instance ToSexp Bool where
    toSexp b = SSymbol $ if b then "t" else "nil"

instance ToSexp a => ToSexp [a] where
    toSexp = SList . map toSexp

instance ToSexp a => ToSexp (Maybe a) where
    toSexp Nothing  = SSymbol "nil"
    toSexp (Just x) = toSexp x

instance ToSexp ModuleName where
    toSexp = toSexp . map C8.pack . ModuleName.components

instance (ToSexp a, ToSexp b, ToSexp c, ToSexp d) => ToSexp (a, b, c, d) where
    toSexp (a, b, c, d) = SList [toSexp a, toSexp b, toSexp c, toSexp d]

cons :: (ToSexp a, ToSexp b) => a -> [b] -> Sexp
cons h t = SList (toSexp h : map toSexp t)

-- | Get possible dist directory
distDir :: TargetTool -> IO FilePath
distDir Cabal = return defaultDistPref
distDir Stack = do
    res <- try $ readProcessWithExitCode "stack" ["path", "--dist-dir"] []
    return $ case res of
        Left (_ :: SomeException)      -> defaultDistDir
        Right (ExitSuccess, stdOut, _) -> stripWhitespace stdOut
        Right (ExitFailure _, _, _)    -> defaultDistDir
  where
    defaultDistDir :: FilePath
    defaultDistDir =
        ".stack-work" </> defaultDistPref
                      </> display buildPlatform
                      </> "Cabal-" ++ cabalVersion'
#if defined(Cabal20OrLater)
distDir (CabalNew packageId ghcVersion) =
    return $ "dist-newstyle/build" </> display buildPlatform
                                   </> "ghc-" ++ ghcVersion
                                   </> display packageId
#endif

getBuildDirectories
    :: TargetTool
    -> PackageDescription
    -> FilePath
    -> IO ([UnixFilepath], [UnixFilepath])
getBuildDirectories tool pkgDesc cabalDir = do
    distDir' <- distDir tool
    let buildDir   :: FilePath
        buildDir   = cabalDir </> distDir' </> "build"

        componentNames :: [String]
        componentNames =
            getExeNames pkgDesc ++
            getTestNames pkgDesc ++
            getBenchmarkNames pkgDesc ++
            getForeignLibNames pkgDesc

    autogenDirs <- getAutogenDirs buildDir componentNames

    let componentBuildDir :: String -> FilePath
        componentBuildDir componentName =
#if defined(Cabal20OrLater)
          case tool of
            CabalNew _ _ -> cabalDir </> distDir'
                                     </> "build"
                                     </> componentName
                                     </> (componentName ++ "-tmp")
            _ -> buildDir </> componentName </> (componentName ++ "-tmp")
#else
          buildDir </> componentName </> (componentName ++ "-tmp")
#endif

        buildDirs :: [FilePath]
        buildDirs =
            autogenDirs ++ map componentBuildDir componentNames

        buildDirs' = case library pkgDesc of
            Just _  -> buildDir : buildDirs
            Nothing -> buildDirs
    return (map mkUnixFilepath buildDirs', map mkUnixFilepath autogenDirs)

getAutogenDirs :: FilePath -> [String] -> IO [FilePath]
getAutogenDirs buildDir componentNames =
    (autogenDir :) A.<$> componentsAutogenDirs buildDir componentNames
  where
    -- 'dist/bulid/autogen' OR
    -- '.stack-work/dist/x86_64-linux/Cabal-1.24.2.0/build/autogen' OR
    -- ./dist-newstyle/build/x86_64-linux/ghc-8.4.3/lens-4.17/build/autogen
    autogenDir :: FilePath
    autogenDir = buildDir </> "autogen"

getSourceDirectories :: [BuildInfo] -> FilePath -> [String]
getSourceDirectories buildInfo cabalDir =
    map (cabalDir </>) (concatMap hsSourceDirs buildInfo)

#if defined(Cabal20OrLater)
doesPackageEnvExist :: GhcVersion -> FilePath -> IO Bool
doesPackageEnvExist ghcVersion projectDir = doesFileExist $ projectDir </> packageEnvFn
  where
    packageEnvFn =
      -- The filename for package environments in MacOS uses the synonym "darwin
      -- "instead of the "official" buildPlatform, "osx".
      case buildOS of
        OSX -> ".ghc.environment." ++ display buildArch ++ "-" ++ "darwin" ++ "-" ++ ghcVersion
        _   -> ".ghc.environment." ++ display buildPlatform ++ "-" ++ ghcVersion

#endif

allowedOptions :: Set C8.ByteString
allowedOptions = S.fromList
    [ "-w"
    , "-fglasgow-exts"
    , "-fpackage-trust"
    , "-fhelpful-errors"
    , "-F"
    , "-cpp"
    ]

allowedOptionPrefixes :: [C8.ByteString]
allowedOptionPrefixes =
    [ "-fwarn-"
    , "-fno-warn-"
    , "-W"
    , "-fcontext-stack="
    , "-firrefutable-tuples"
    , "-D"
    , "-U"
    , "-I"
    , "-fplugin="
    , "-fplugin-opt="
    , "-pgm"
    , "-opt"
    ]

forbiddenOptions :: Set C8.ByteString
forbiddenOptions = S.fromList
   [ "-Wmissing-home-modules"
   , "-Werror=missing-home-modules"
   ]

isAllowedOption :: C8.ByteString -> Bool
isAllowedOption opt =
    S.member opt allowedOptions || any (`C8.isPrefixOf` opt) allowedOptionPrefixes && S.notMember opt forbiddenOptions

dumpPackageDescription :: PackageDescription -> FilePath -> IO Sexp
dumpPackageDescription pkgDesc projectDir = do
    (cabalDirs, cabalAutogen) <- getBuildDirectories Cabal pkgDesc projectDir
    (stackDirs, stackAutogen) <- getBuildDirectories Stack pkgDesc projectDir
#if defined(Cabal20OrLater)
    ghcVersion <- getGhcVersion
    (cabalNewDirs, cabalNewAutogen) <- getBuildDirectories (CabalNew (package pkgDesc) ghcVersion) pkgDesc projectDir
    packageEnvExists <- doesPackageEnvExist ghcVersion projectDir
    let buildDirs   = cabalDirs ++ stackDirs ++ cabalNewDirs
        autogenDirs = cabalAutogen ++ stackAutogen ++ cabalNewAutogen
#else
    let buildDirs   = cabalDirs ++ stackDirs
        autogenDirs = cabalAutogen ++ stackAutogen
#endif
    let packageName = C8.pack $ unPackageName' $ pkgName $ package pkgDesc
    return $
        SList
            [ cons (sym "build-directories") (ordNub (buildDirs :: [UnixFilepath]))
            , cons (sym "source-directories") (sourceDirs :: [UnixFilepath])
            , cons (sym "extensions") exts
            , cons (sym "languages") langs
            , cons (sym "dependencies") deps
            , cons (sym "other-options") (cppOpts ++ ghcOpts)
            , cons (sym "autogen-directories") (autogenDirs :: [UnixFilepath])
            , cons (sym "should-include-version-header") [not ghcIncludesVersionMacro]
#if defined(Cabal20OrLater)
            , cons (sym "package-env-exists") [packageEnvExists]
#endif
            , cons (sym "package-name") [packageName]
            , cons (sym "components") $ getComponents packageName pkgDesc
            ]
  where
    buildInfo :: [BuildInfo]
    buildInfo = allBuildInfo pkgDesc

    sourceDirs :: [UnixFilepath]
    sourceDirs = ordNub (map mkUnixFilepath (getSourceDirectories buildInfo projectDir))

    exts :: [Extension]
    exts = nub (concatMap usedExtensions buildInfo)

    langs :: [Language]
    langs = nub (concatMap allLanguages buildInfo)

    thisPackage :: PackageName
    thisPackage = pkgName (package pkgDesc)

    deps :: [Dependency]
    deps =
        nub (filter ((/= thisPackage) . depPkgName') (buildDepends' pkgDesc))

    -- The "cpp-options" configuration field.
    cppOpts :: [C8.ByteString]
    cppOpts =
        ordNub (filter isAllowedOption (map C8.pack (concatMap cppOptions buildInfo)))

    -- The "ghc-options" configuration field.
    ghcOpts :: [C8.ByteString]
    ghcOpts =
        ordNub (filter isAllowedOption (map C8.pack (concatMap (hcOptions GHC) buildInfo)))

#if defined(Cabal20OrLater)
    -- We don't care about the stack ghc compiler because we don't need it for
    -- the stack checker
    getGhcVersion :: IO String
    getGhcVersion =
        go "cabal"
           ["new-exec", "ghc", "--", "--numeric-version"]
           (go "ghc" ["--numeric-version"] A.empty)
      where
        go :: String -> [String] -> IO String -> IO String
        go comm opts cont = do
          res <- try $ readProcessWithExitCode comm opts []
          case res of
              Left (_ :: SomeException)      -> cont
              Right (ExitSuccess, stdOut, _) -> return $ stripWhitespace stdOut
              Right (ExitFailure _, _, _)    -> cont
#endif

data ComponentType
    = CTLibrary
    | CTForeignLibrary
    | CTExecutable
    | CTTestSuite
    | CTBenchmark
    deriving (Eq, Ord, Show, Enum, Bounded)

componentTypePrefix :: ComponentType -> C8.ByteString
componentTypePrefix x = case x of
    CTLibrary        -> "lib"
    CTForeignLibrary -> "flib"
    CTExecutable     -> "exe"
    CTTestSuite      -> "test"
    CTBenchmark      -> "bench"

instance ToSexp ComponentType where
    toSexp = toSexp . componentTypePrefix

-- | Gather files and modules that constitute each component.
getComponents :: C8.ByteString -> PackageDescription -> [(ComponentType, C8.ByteString, Maybe C8.ByteString, [ModuleName])]
getComponents packageName pkgDescr =
    [ (CTLibrary, name, Nothing, exposedModules lib ++ biMods bi)
    | lib <- allLibraries' pkgDescr
    , let bi = libBuildInfo lib
    , let name = maybe packageName C8.pack $ libName' lib
    ] ++
#if defined(Cabal20OrLater)
    [ (CTForeignLibrary, C8.pack (foreignLibName' flib), Nothing, biMods bi)
    | flib <- foreignLibs pkgDescr
    , let bi = foreignLibBuildInfo flib
    ] ++
#endif
    [ (CTExecutable, C8.pack (exeName' exe), Just (C8.pack (modulePath exe)), biMods bi)
    | exe <- executables pkgDescr
    , let bi = buildInfo exe
    ] ++
    [ (CTTestSuite, C8.pack (testName' tst), exeFile, maybeToList extraMod ++ biMods bi)
    | tst <- testSuites pkgDescr
    , let bi = testBuildInfo tst
    , let (exeFile, extraMod) = case testInterface tst of
            TestSuiteExeV10 _ path    -> (Just (C8.pack path), Nothing)
            TestSuiteLibV09 _ modName -> (Nothing, Just modName)
            TestSuiteUnsupported{}    -> (Nothing, Nothing)
    ]
#ifdef Cabal114OrMore
    ++
    [ (CTBenchmark, C8.pack (benchmarkName' tst), exeFile, biMods bi)
    | tst <- benchmarks pkgDescr
    , let bi = benchmarkBuildInfo tst
    , let exeFile = case benchmarkInterface tst of
            BenchmarkExeV10 _ path -> Just (C8.pack path)
            BenchmarkUnsupported{} -> Nothing
    ]
#endif
  where
#if defined(Cabal20OrLater)
      biMods bi =
          otherModules bi
#if defined(Cabal22OrLater)
          ++ virtualModules bi
#endif
          ++ autogenModules bi
#else
      biMods = otherModules
#endif

getCabalConfiguration :: HPackExe -> ConfigurationFile -> IO Sexp
getCabalConfiguration hpackExe configFile = do
    genericDesc <-
        case configFile of
            HPackFile path -> readHPackPkgDescr hpackExe path projectDir
            CabalFile path -> readGenericPkgDescr path
    case getConcretePackageDescription genericDesc of
        Left e        -> die' $ "Issue with package configuration\n" ++ show e
        Right pkgDesc -> dumpPackageDescription pkgDesc projectDir
  where
    projectDir :: FilePath
    projectDir = dropFileName $ configFilePath configFile

readHPackPkgDescr :: HPackExe -> FilePath -> FilePath -> IO GenericPackageDescription
readHPackPkgDescr exe configFile projectDir = do
    (Nothing, Just out, Just err, procHandle) <- Process.createProcess p
    cabalFileContents <- readCabalFileContentsFromHandle out
    exitCode <- Process.waitForProcess procHandle
    case exitCode of
        ExitFailure{} -> do
            err' <- hGetContents err
            die' $ "Failed to obtain cabal configuration by running hpack on '" ++ configFile ++ "':\n" ++ err'
        ExitSuccess ->
            case parsePkgDescr "<generated by hpack>" cabalFileContents of
                Left msgs ->
                    die' $ "Failed to parse cabal file produced by hpack from '" ++ configFile ++ "':\n" ++
                        unlines msgs
                Right x   -> return x
  where
    p = (Process.proc (unHPackExe exe) [configFile, "-"])
        { Process.std_in  = Process.Inherit
        , Process.std_out = Process.CreatePipe
        , Process.std_err = Process.CreatePipe
        , Process.cwd     = Just projectDir
        }

buildDepends' :: PackageDescription -> [Dependency]
buildDepends' =
#if defined(Cabal24) || defined(Cabal30) || defined(Cabal32)
    allBuildDepends
#else
    buildDepends
#endif

readGenericPkgDescr :: FilePath -> IO GenericPackageDescription
readGenericPkgDescr =
#if defined(Cabal20OrLater)
    readGenericPackageDescription silent
#else
    readPackageDescription silent
#endif

newtype CabalFileContents = CabalFileContents
    { unCabalFileContents ::
#if defined(Cabal22OrLater)
        BS.ByteString
#else
        String
#endif
    }

readCabalFileContentsFromHandle :: Handle -> IO CabalFileContents
readCabalFileContentsFromHandle =
    fmap CabalFileContents .
#if defined(Cabal22OrLater)
        BS.hGetContents
#else
        hGetContents
#endif

parsePkgDescr :: FilePath -> CabalFileContents -> Either [String] GenericPackageDescription
parsePkgDescr _fileName cabalFileContents =
#if defined(Cabal32OrLater)
    case runParseResult $ parseGenericPackageDescription $ unCabalFileContents cabalFileContents of
        (_warnings, res) ->
            case res of
                Left (_version, errs) -> Left $ map (showPError _fileName) $ toList errs
                Right x -> return x
#elif defined(Cabal22OrLater)
    case runParseResult $ parseGenericPackageDescription $ unCabalFileContents cabalFileContents of
        (_warnings, res) ->
            case res of
                Left (_version, errs) -> Left $ map (showPError _fileName) errs
                Right x -> return x
#elif defined(Cabal20)
    case parseGenericPackageDescription $ unCabalFileContents cabalFileContents of
        ParseFailed failure ->
            let (_line, msg) = locatedErrorMsg failure
            in Left [msg]
        ParseOk _warnings x  -> Right x
#else
    case parsePackageDescription $ unCabalFileContents cabalFileContents of
        ParseFailed failure ->
            let (_line, msg) = locatedErrorMsg failure
            in Left [msg]
        ParseOk _warnings x  -> Right x
#endif

getConcretePackageDescription
    :: GenericPackageDescription
    -> Either [Dependency] PackageDescription
getConcretePackageDescription genericDesc = do
#if defined(Cabal22OrLater)
    let enabled :: ComponentRequestedSpec
        enabled = ComponentRequestedSpec
            { testsRequested      = True
            , benchmarksRequested = True
            }
    fst A.<$> finalizePD
        (mkFlagAssignment []) -- Flag assignment
        enabled               -- Enable all components
        (const True)          -- Whether given dependency is available
        buildPlatform
        buildCompilerId
        []                    -- Additional constraints
        genericDesc
#elif defined(Cabal20)
    let enabled :: ComponentRequestedSpec
        enabled = ComponentRequestedSpec
            { testsRequested      = True
            , benchmarksRequested = True
            }
    fst A.<$> finalizePD
        []           -- Flag assignment
        enabled      -- Enable all components
        (const True) -- Whether given dependency is available
        buildPlatform
        buildCompilerId
        []           -- Additional constraints
        genericDesc
#elif Cabal114OrMore
     -- This let block is eerily like one in Cabal.Distribution.Simple.Configure
     let enableTest :: TestSuite -> TestSuite
         enableTest t = t { testEnabled = True }
         enableBenchmark :: Benchmark -> Benchmark
         enableBenchmark bm = bm { benchmarkEnabled = True }
         flaggedTests =
             map (second (mapTreeData enableTest)) (condTestSuites genericDesc)
         flaggedBenchmarks =
             map
                 (second (mapTreeData enableBenchmark))
                 (condBenchmarks genericDesc)
         genericDesc' =
             genericDesc
             { condTestSuites = flaggedTests
             , condBenchmarks = flaggedBenchmarks
             }
     fst A.<$> finalizePackageDescription
         []
         (const True)
         buildPlatform
         buildCompilerId
         []
         genericDesc'
#else
     -- This let block is eerily like one in Cabal.Distribution.Simple.Configure
     let enableTest :: TestSuite -> TestSuite
         enableTest t = t { testEnabled = True }
         flaggedTests =
             map (second (mapTreeData enableTest)) (condTestSuites genericDesc)
         genericDesc' =
             genericDesc
             { condTestSuites = flaggedTests
             }
     fst A.<$> finalizePackageDescription
         []
         (const True)
         buildPlatform
         buildCompilerId
         []
         genericDesc'
#endif

componentsAutogenDirs :: FilePath -> [String] -> IO [FilePath]
#if defined(Cabal20OrLater)
componentsAutogenDirs buildDir componentNames =
        filterM doesDirectoryExist $
            map (\path -> buildDir </> path </> "autogen") componentNames
#else
componentsAutogenDirs _ _ = return []
#endif

#if defined(Cabal118OrLess)
buildCompilerId :: CompilerId
buildCompilerId = CompilerId buildCompilerFlavor compilerVersion
#else
buildCompilerId :: CompilerInfo
buildCompilerId = unknownCompilerInfo compId NoAbiTag
  where
    compId :: CompilerId
    compId = CompilerId buildCompilerFlavor compVersion
# if defined(Cabal20OrLater)
    compVersion :: CabalVersion.Version
    compVersion = CabalVersion.mkVersion' compilerVersion
# else
    compVersion :: Version
    compVersion = compilerVersion
# endif
#endif

allLibraries' :: PackageDescription -> [Library]
allLibraries' =
#if defined(Cabal20OrLater)
    allLibraries
#else
    maybeToList . library
#endif

libName' :: Library -> Maybe String
libName' =
#if defined(Cabal30OrLater)
    fmap unUnqualComponentName . libraryNameString . libName
#elif defined(Cabal20) || defined(Cabal22) || defined(Cabal24)
    fmap unUnqualComponentName . libName
#else
    const Nothing
#endif

exeName' :: Executable -> String
exeName' =
#if defined(Cabal20OrLater)
    unUnqualComponentName . exeName
#else
    exeName
#endif

#if defined(Cabal20OrLater)
foreignLibName' :: ForeignLib -> String
foreignLibName' =
    unUnqualComponentName . foreignLibName
#endif

testName' :: TestSuite -> String
testName' =
#if defined(Cabal20OrLater)
    unUnqualComponentName . testName
#else
    testName
#endif

#ifdef Cabal114OrMore
benchmarkName' :: Benchmark -> FilePath
benchmarkName' =
# if defined(Cabal20OrLater)
    unUnqualComponentName . benchmarkName
# else
    benchmarkName
# endif
#endif


getExeNames :: PackageDescription -> [String]
getExeNames =
    map exeName' . executables

getForeignLibNames :: PackageDescription -> [String]
getForeignLibNames =
#if defined(Cabal20OrLater)
    map foreignLibName' . foreignLibs
#else
    const []
#endif

getTestNames :: PackageDescription -> [String]
getTestNames =
    map testName' . testSuites

getBenchmarkNames :: PackageDescription -> [String]
getBenchmarkNames =
#ifdef Cabal114OrMore
    map benchmarkName' . benchmarks
#else
    const []
#endif

depPkgName' :: Dependency -> PackageName
depPkgName' =
#if defined(Cabal20OrLater)
    depPkgName
#else
    let f (Dependency x _) = x in f
#endif

unPackageName' :: PackageName -> String
unPackageName' =
#if defined(Cabal20OrLater)
    unPackageName
#else
    let f (PackageName x) = x in f
#endif


-- Textual representation of cabal version
cabalVersion' :: String
cabalVersion' =
#if defined(Cabal22OrLater)
    prettyShow cabalVersion
#elif defined(Cabal20)
    CabalVersion.showVersion cabalVersion
#else
    showVersion cabalVersion
#endif

ghcIncludesVersionMacro :: Bool
ghcIncludesVersionMacro =
#if defined(GHC_INCLUDES_VERSION_MACRO)
    True
#else
    False
#endif

ordNub :: forall a. Ord a => [a] -> [a]
ordNub = go S.empty
  where
    go :: Set a -> [a] -> [a]
    go _   []     = []
    go !acc (x:xs)
        | S.member x acc = go acc xs
        | otherwise      = x : go (S.insert x acc) xs

stripWhitespace :: String -> String
stripWhitespace = reverse . dropWhile isSpace . reverse . dropWhile isSpace

die' :: String -> IO a
die' msg = do
    hPutStrLn stderr msg
    exitFailure

#if !defined(HAVE_DATA_FUNCTOR_IDENTITY)
newtype Identity a = Identity { runIdentity :: a }
#endif

data ConfigurationFile =
      CabalFile FilePath
    | HPackFile FilePath

configFilePath :: ConfigurationFile -> FilePath
configFilePath (CabalFile path) = path
configFilePath (HPackFile path) = path

newtype HPackExe = HPackExe { unHPackExe :: FilePath }

data Config f = Config
    { cfgInputFile :: f ConfigurationFile
    , cfgHPackExe  :: HPackExe
    }

reifyConfig :: Config Maybe -> IO (Config Identity)
reifyConfig Config{cfgInputFile, cfgHPackExe} = do
    cfgInputFile' <- case cfgInputFile of
        Nothing   -> die' "Input file not specified. Use --cabal-file or --hpack-file to specify one."
        Just path -> return path
    return Config
        { cfgInputFile = Identity cfgInputFile'
        , cfgHPackExe
        }

optionDescr :: [OptDescr (Config Maybe -> Config Maybe)]
optionDescr =
    [ Option
          []
          ["cabal-file"]
          (ReqArg (\path cfg -> cfg { cfgInputFile = Just (CabalFile path) }) "FILE")
          "Cabal file to process"
    , Option
          []
          ["hpack-file"]
          (ReqArg (\path cfg -> cfg { cfgInputFile = Just (HPackFile path) }) "FILE")
          "HPack package.yaml file to process"
    , Option
          []
          ["hpack-exe"]
          (ReqArg (\path cfg -> cfg { cfgHPackExe = HPackExe path }) "FILE")
          "Path to 'hpack' executable"
    ]

defaultConfig :: Config Maybe
defaultConfig = Config
    { cfgInputFile = Nothing
    , cfgHPackExe  = HPackExe "hpack"
    }

main' :: Config Identity -> IO ()
main' Config{cfgInputFile, cfgHPackExe} =
    hPutBuilder stdout . renderSexp =<<
        getCabalConfiguration cfgHPackExe (runIdentity cfgInputFile)

main :: IO ()
main = do
    args <- getArgs
    when (any (`elem` ["-h", "--help"]) args) $ do
        putStrLn usage
        exitSuccess
    case getOpt' RequireOrder optionDescr args of
        (fs, [],  [],  []) -> do
            let cfg = foldl' (flip ($)) defaultConfig fs
            main' =<< reifyConfig cfg
        (_,  x:_, [],  []) ->
            die' $ "Unrecognised argument: " ++ x
        (_,  [],  y:_, []) ->
            die' $ "Unrecognised command-line option: " ++ y
        (_,  _,   _,   es) ->
            die' $ "Failed to parse command-line options:\n" ++ unlines es
  where
    header = "Usage: get-cabal-configuration [OPTION...]"
    usage = usageInfo header optionDescr
