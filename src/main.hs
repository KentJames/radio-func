{-# LANGUAGE OverloadedStrings #-}

import System.Environment
import Control.Applicative -- <$> <*>
import Data.Time.Format
import Data.Maybe (fromJust)
import Data.Yaml
{-
 This file interprets a configuration file in a set YAML format that describes:
 1) The Sky
 2) The Telescope
 3) The observation parameters
-}
import qualified Data.ByteString.Char8 as BS
--import Data.ByteString.Lazy.UTF8 as BLU -- from utf8-string
--import Data.ByteString.UTF8 as BSU      -- from utf8-string

{- My imports -}
import Geometry
import Source
import Time
import InterfTypes

  
main :: IO()
main = do
  args <- getArgs
  print args
  config <- BS.readFile(args !! 0)

  BS.putStrLn  $ BS.unlines ["Config File: ",config]
  -- let decoded = readMyConfig
  let paths = Data.Yaml.decodeThrow config :: Maybe Config
  print paths
  
--  let path = decoded.Paths.rootdir
  let obj = fromJust paths
--  print obj
  let odir = source_locs (sources obj)
  print odir
  let starttimeio = starttime (observation obj)
  let endtimeio = endtime (observation obj)
  print starttimeio
  let starttimelst =  (convert_jd_to_lst . convert_datestring_to_jd) starttimeio 0.0 
  let endtimelst = (convert_jd_to_lst . convert_datestring_to_jd) endtimeio 0.0
  print starttimelst
  print endtimelst
  let starttimelst_double = lst_to_double starttimelst
  let endtimelst_double = lst_to_double endtimelst
  print starttimelst_double
  print endtimelst_double

  
  let obs_days = datestring_get_val endtimeio 0 - datestring_get_val starttimeio 0
  let tinstances = observation_time_instances starttimelst endtimelst (accumulation (observation obj)) obs_days
  
  print "Starting simulator..."

  
  
