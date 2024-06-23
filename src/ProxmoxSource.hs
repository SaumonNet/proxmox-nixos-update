{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module ProxmoxSource where

import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (splitOn, strip)
import qualified Data.Text.IO
import OurPrelude
import System.IO

data Package = Package
  { name :: Text,
    versions :: [Text]
  }
  deriving (Show)

dedupPackages :: [(Text, [Text])] -> [Package]
dedupPackages inp =
  let a = HM.fromListWith (++) inp
   in map (\(k, v) -> Package {name = k, versions = v}) (HM.toList a)

parseVersion :: Text -> Maybe Text
parseVersion str = case splitOn "-" str of
  [version, _] -> Just version
  [version] -> Just version
  _ -> Nothing

parseKeyValue :: Text -> Maybe (Text, Text)
parseKeyValue str =
  case splitOn ":" str of
    [key, value] -> Just (strip key, strip value)
    _ -> Nothing

parsePackage :: Text -> Maybe (Text, [Text])
parsePackage inp =
  let hm = HM.fromList $ mapMaybe parseKeyValue (splitOn "\n" inp)
   in do
        p_name <- HM.lookup "Package" hm
        p_version <- HM.lookup "Version" hm
        p_version_parsed <- parseVersion p_version
        return (p_name, [p_version_parsed])

splitAllPackages :: Text -> [Text]
splitAllPackages = splitOn "\n\n"

fetch :: IO ()
fetch = do
  packages <- Data.Text.IO.readFile "/home/julien/Downloads/Packages"
  let a = splitAllPackages packages
  let b = mapMaybe parsePackage a
  let c = dedupPackages b
  let d = map (\p -> (name p, maximum (versions p))) c
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  liftIO $ hPutStrLn stderr "starting"
  liftIO $ hPutStrLn stderr $ show d
  return ()
