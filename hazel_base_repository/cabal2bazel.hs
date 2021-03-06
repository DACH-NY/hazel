-- | A program for reifying a .cabal file into a Bazel structure.
--
-- Takes as input a Cabal file and a GHC version.
-- Evaluates all of the flags in the file to generate a PackageDescription,
-- and then writes out a `.bzl` file containing a single definition:
--
--    package = struct(...)
--
-- which can be loaded into Bazel BUILD files.
{-# LANGUAGE CPP #-}
{-# LANGUAGE LambdaCase #-}
module Main (main) where

#if MIN_VERSION_Cabal(2,2,0)
import Distribution.PackageDescription.Parsec
    (readGenericPackageDescription, parseHookedBuildInfo, runParseResult)
import qualified Data.ByteString.Char8 as BS.C8
#else
import Distribution.PackageDescription.Parse
    (readGenericPackageDescription, parseHookedBuildInfo, ParseResult(..))
#endif

import Distribution.Text (display, simpleParse)
import Distribution.Verbosity (normal)
import System.Environment (getArgs)
import System.FilePath ((<.>))
import System.Process (callProcess)

import qualified Data.Map.Strict as Map
import qualified Distribution.Package as P
import qualified Distribution.PackageDescription as P
import qualified System.Directory as Directory

import Description
import Flatten
import Skylark

main :: IO ()
main = do
    ghcVersionStr:cabalFile:outFile:flagArgs <- getArgs
    gdesc <- readGenericPackageDescription normal cabalFile
    let ghcVersion = case simpleParse ghcVersionStr of
                      Nothing -> error $ "Error parsing ghc version: " ++ show ghcVersionStr
                      Just v -> v
        packageFlags = parseFlags flagArgs
    desc <- maybeConfigure $ flattenToDefaultFlags ghcVersion packageFlags gdesc
    writeFile outFile $ show $ renderStatements
        [Assign "package" $ packageDescriptionExpr desc]

parseFlags :: [String] -> Map.Map P.FlagName Bool
parseFlags = \case
  "-flag-on":flag:etc -> Map.insert (P.mkFlagName flag) True (parseFlags etc)
  "-flag-off":flag:etc -> Map.insert (P.mkFlagName flag) False (parseFlags etc)
  _ -> Map.empty

maybeConfigure :: P.PackageDescription -> IO P.PackageDescription
maybeConfigure desc = whenConfiguring $ do
    callProcess "./configure" []
    let buildInfoFile = display (P.packageName desc) <.> "buildinfo"
    buildInfoExists <- Directory.doesFileExist buildInfoFile
    if buildInfoExists
      then processBuildInfoFile buildInfoFile
      else return desc
  where
#if MIN_VERSION_Cabal(2,2,0)
    whenConfiguring m = case P.buildType desc of
      P.Configure -> m
      _           -> return desc

    processBuildInfoFile buildInfoFile = do
      cs <- BS.C8.readFile buildInfoFile
      case runParseResult (parseHookedBuildInfo cs) of
        (_warnings, Left (_maybeVersion, es)) ->
          error $ "Error reading buildinfo " ++ show buildInfoFile ++ ": " ++ show es
        (_warnings, Right hookedBI) ->
          return $ P.updatePackageDescription hookedBI desc
#else
    whenConfiguring m = case P.buildType desc of
      Just P.Configure -> m
      _                -> return desc

    processBuildInfoFile buildInfoFile = do
      cs <- readFile buildInfoFile
      case parseHookedBuildInfo cs of
          ParseFailed e -> error $ "Error reading buildinfo " ++ show buildInfoFile
                                      ++ ": " ++ show e
          ParseOk _ hookedBI -> return $ P.updatePackageDescription hookedBI desc
#endif
