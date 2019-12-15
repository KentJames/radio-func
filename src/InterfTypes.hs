{-# LANGUAGE OverloadedStrings #-}
module InterfTypes where

import Control.Applicative -- <$> <*>
import Data.Yaml
import Data.Maybe (fromJust)
import qualified Data.ByteString.Char8 as BS


data Config = Config {
  paths :: Paths,
  simulation :: Simulation,
  observation :: Observation,
  telescope :: Telescope,
  interferometer :: Interferometer,
  sources :: Sources} deriving (Show)

data Paths = Paths {
  rootdir :: String,
  outputdir :: String} deriving (Show)

data Simulation = Simulation {
  voltage_sim :: Bool} deriving (Show)

data Observation = Observation {
  starttime :: String,
  endtime :: String,
  accumulation :: Double,
  phase_center :: [Double],
  phase_coords :: String} deriving (Show)

{-
  Defines the locations of each individual interferometer element in the local co-ordinate system.
 -}

data Interferometer = Interferometer {
  locations_x :: [Double],
  locations_y :: [Double],
  locations_z :: [Double]} deriving (Show)

{-
  Describes location of co-ordinate centre of the array, its frequency channels
  and noise properties.
-}

data Telescope = Telescope {
  latitude :: Double,
  longitude :: Double,
  frequency :: [Double],
  tsys :: Double} deriving (Show)

data Sources = Sources {
  source_locs :: [[Double]],
  flat_spectrum :: Bool,
  spectrum_file :: String} deriving (Show)

instance FromJSON Config where
  parseJSON (Object v) = Config <$>
                         v .: "paths" <*>
                         v .: "simulation" <*>
                         v .: "observation" <*>
                         v .: "telescope" <*>
                         v .: "interferometer" <*>
                         v .: "sources" 
  parseJSON _ = error "Parsing YAML Observation parameters file failed."

instance FromJSON Paths where
  parseJSON (Object v) = Paths <$>
                         v .: "rootdir" <*>
                         v .: "outputdir"
  parseJSON _ = error "Can't Parse Paths object."

instance FromJSON Simulation where
  parseJSON (Object v) = Simulation <$>
                         v .: "voltage_sim"
  parseJSON _ = error "Can't parse simulation object."

instance FromJSON Observation where
  parseJSON (Object v) = Observation <$>
                         v .: "startime" <*>
                         v .: "endtime" <*>
                         v .: "accumulation" <*>
                         v .: "phase_center" <*>
                         v .: "phase_coords"
  parseJSON _ = error "Can't Parse Observation object."

instance FromJSON Telescope where
  parseJSON (Object v) = Telescope <$>
                         v .: "latitude" <*>
                         v .: "longitude" <*>
                         v .: "frequency" <*>
                         v .: "tsys" 
  parseJSON _ = error "Can't Parse Telescope object."

instance FromJSON Interferometer where
  parseJSON (Object v) = Interferometer <$>
                         v .: "locations_x" <*>
                         v .: "locations_y" <*>
                         v .: "locations_z"
  parseJSON _ = error "Can't Parse Interferometer object."

instance FromJSON Sources where
  parseJSON (Object v) = Sources <$>
    v .: "source_locs" <*>
    v .: "flat_spectrum" <*>
    v .: "spectrum_file"
  parseJSON _ = error "Can't Parse Sources object."
