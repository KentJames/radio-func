module Time where

import Data.Astro.Time.Sidereal as ST
import Data.Astro.Time.JulianDate
import Data.Astro.Types

-- convert_datestamp_to_lst :: String -> Float
-- convert_datestamp_to_lst datestamp = ST.dhToLST dh
--   where dh = 

readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
                  [(val, "")] -> Just val
                  _           -> Nothing

convert_datestring_to_jd :: String -> JulianDate
convert_datestring_to_jd datestring = fromYMDHMS  year month day hour minute second
  where dl = words datestring
        year = (read (dl!!2))
        month = (read (dl!!1))
        day = (read (dl!!0))
        hour = (read (dl!!3))
        minute = (read (dl!!4))
        second = (read (dl!!5))


datestring_get_val :: String -> Int -> Int
datestring_get_val datestring val = read $ dl!!val
  where dl = words datestring
  
convert_jd_to_lst :: JulianDate -> Double ->  LocalSiderealTime
convert_jd_to_lst juliandate longitude = gstToLST (fromRadians longitude) (utToGST juliandate)

{-
 Necessary conversion to play nicely with Geometry module
-}
lst_to_double :: LocalSiderealTime -> Double
lst_to_double lst = (toRadians (fromDecimalHours (lstToDH lst))) / ((2 * pi)/24.0)

seconds_to_frachours :: Double -> Double
seconds_to_frachours seconds = seconds/3600.0

observation_time_instances :: LocalSiderealTime -> LocalSiderealTime -> Double -> Int ->  [Double]
observation_time_instances start end integration days = [startlst,listiterval..endlst]
  where startlst = lst_to_double start
        endlst = lst_to_double end + (24.0 * fromIntegral days)
        listiterval = startlst + seconds_to_frachours integration



