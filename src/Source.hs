module Source(Source,
             get_spectrum,
             create_source)
where

data Source = Source { location :: (Float,Float)
                     , spectrum :: [Float]
                     , frequency :: [Float] } deriving(Show)


get_spectrum :: Source -> ([Float],[Float])
get_spectrum (Source location spectrum frequency)
  | length(spectrum) == length(frequency) = (spectrum, frequency)
  | otherwise = error "Spectrum and frequency are not the same length!"

create_spectrum :: Float -> Float -> Int -> [Float]
create_spectrum lower step length = [lower + step* fromIntegral(i) | i <- [0..length]]

create_flat_spectrum :: Int -> [Float]
create_flat_spectrum length = replicate length 1

create_source :: (Float, Float) -> Float -> Int -> Float -> String -> Source
create_source (ra,dec) frequency channels channel_width "flat" =
  Source {location=(ra,dec), spectrum=create_flat_spectrum channels, frequency=create_spectrum frequency channel_width channels}

-- derive_time_series_voltage :: [Float] -> [Float]
-- derive_time_series_voltage = 


{--generate_flat_spectrum :: (Float,Float) -> (Float,Float) -> Float ->  Source
generate_flat_spectrum (a,b) loc channels
  | a < b = Source{ location=loc, spectrum = array (a,b) [(i,i*i) | [a,b]
--}

