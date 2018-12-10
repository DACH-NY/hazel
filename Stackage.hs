#!/usr/bin/env stack
-- stack --resolver lts-10.5 script --package base16-bytestring --package cryptohash --package http-types --package http-client --package http-client-tls --package aeson --package bytestring --package text --package containers --package temporary --package process --package pretty --package Cabal --package yaml --ghc-options=-ihazel_base_repository
--
-- This script generates a .bzl file containing the versions of all packages in a
-- particular Stackage LTS release.
--
-- To invoke:
--     ./Stackage.hs lts-10.5 packages.bzl
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Main (main) where

import qualified Crypto.Hash.SHA256 as SHA256
import Control.Monad (forM_, unless)
import qualified Data.Aeson as Aeson
import Control.Lens hiding ((<.>))
import Data.Aeson.Lens
import Data.Aeson.Types
import Data.Bifunctor
import Data.Yaml
import Distribution.Package
import Distribution.PackageDescription (FlagName, mkFlagName, unFlagName)
import Distribution.Version
import System.Environment (getArgs)
import System.FilePath
import Control.Exception (throw)
import System.IO
import System.IO.Temp
import Text.PrettyPrint (($$), (<+>), text)

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Base16 as Base16
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map.Strict as Map
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Distribution.Text as Cabal

import Skylark

main :: IO ()
main = do
    [ltsYamlPath, allCabalHashesDir, out] <- getArgs

    ltsYaml <- B.readFile ltsYamlPath
    plan <- case decodeEither' ltsYaml of
        Left err -> throw err
        Right (x :: BuildPlan) -> pure x
    shas <- flip Map.traverseWithKey (planPackages plan) $ \n p -> do
        cabalPackageSHA256 allCabalHashesDir n p
    writeFile out $ show $
        "# Generated from Stackage.hs with sha256:" <+>
        -- why is this base16 encoded?
          text (BC.unpack $ Base16.encode (SHA256.hash ltsYaml))
        $$ renderStatements
          [ Assign "core_packages" $ expr $ corePackageList plan
          , Assign "packages" $ expr $ packageList shas plan
          ]

cabalPackageSHA256 :: FilePath -> PackageName -> PlanPackage -> IO B.ByteString
cabalPackageSHA256 allCabalHashesDir n p  = do
    let
      jsonPath = allCabalHashesDir </>
        Cabal.display n </>
        Cabal.display (planPackageVersion p) </>
        Cabal.display n <.> "json"
    package <- L.readFile jsonPath
    case T.encodeUtf8 <$> package ^? key "package-hashes" . key "SHA256" . _String of
      Just x -> pure x
      Nothing -> error $ "can't find hash in " ++ show package

packageList :: Map.Map PackageName B.ByteString -> BuildPlan -> [(String, Expr)]
packageList shas = map mk . Map.toList . planPackages
  where
    mk (n, p) =
      ( Cabal.display n
      , ExprCall "struct" $
        [ "version" =: (Cabal.display $ planPackageVersion p)
        , "sha256" =: BC.unpack (Base16.encode (shas Map.! n))
        ] ++
        let flags = planPackageFlags p
        in if Map.null flags
           then []
           else ["flags" =: flagsExpr flags]
      )

corePackageList :: BuildPlan -> [(String, String)]
corePackageList = map mk . Map.toList . corePackageVersions
  where
    mk (n, v) = (Cabal.display n, Cabal.display v)

flagsExpr :: Flags -> Expr
flagsExpr m = ExprDict $
  bimap (ExprString . unFlagName) ExprBool <$> Map.toList m

--------------------------------------------------------------------------------
-- JSON data types and instances for parsing the LTS yaml file
--
newtype PlanName = PlanName { renderPlanName :: String }
    deriving Show

data BuildPlan = BuildPlan
    { corePackageVersions :: Map.Map PackageName Version
    , planPackages :: Map.Map PackageName PlanPackage
    , ghcVersion :: Version
    } deriving Show

data PlanPackage = PlanPackage
    { planPackageVersion :: Version
    , planPackageFlags :: Flags
    } deriving Show

type Flags = Map.Map FlagName Bool

instance FromJSON Version where
    parseJSON = withText "Version" simpleParser

instance FromJSONKey Version where
    fromJSONKey = cabalKeyTextParser

instance FromJSON PackageName where
    parseJSON = withText "PackageName" simpleParser

instance FromJSONKey PackageName where
    fromJSONKey = cabalKeyTextParser

instance FromJSON BuildPlan where
    parseJSON = withObject "Plan" $ \o -> do
        sys <- o .: "system-info"
        coreVersions <- sys .: "core-packages"
        ghcVers <- sys .: "ghc-version"
        pkgs <- o .: "packages"
        return BuildPlan { corePackageVersions = coreVersions
                         , planPackages = pkgs
                         , ghcVersion = ghcVers
        }

instance FromJSON PlanPackage where
    parseJSON = withObject "PlanPackage" $ \o ->
        PlanPackage <$> (o .: "version") <*> ((o .: "constraints") >>= (.: "flags"))

instance FromJSON FlagName where
    parseJSON = fmap mkFlagName . parseJSON

instance FromJSONKey FlagName where
    fromJSONKey = FromJSONKeyText (mkFlagName . T.unpack)

instance FromJSON PlanName where
    parseJSON = fmap PlanName . parseJSON


simpleParser :: Cabal.Text a => T.Text -> Parser a
simpleParser t = case Cabal.simpleParse (T.unpack t) of
                        Just v -> pure v
                        Nothing -> fail $ "Unable to parse: "
                                            ++ show t

cabalKeyTextParser :: Cabal.Text a => FromJSONKeyFunction a
cabalKeyTextParser = FromJSONKeyTextParser simpleParser
