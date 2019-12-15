module Geometry (altaz_to_dircos,
                 dircos_to_altaz,
                 hadec_to_altaz,
                 altaz_to_hadec,
                 hadec_to_radec,
                 radec_to_hadec,
                 calculate_geometric_delay,
                 xyz_to_uvw,
                 uvw_to_xyz) where

altaz_to_dircos :: (Double, Double) -> (Double, Double)
altaz_to_dircos (alt, az)
  | (alt < pi/2) = (cos(alt)*cos(phi), cos(alt)*sin(phi))
  | otherwise = (0,0)
  where phi = pi/2 - az

dircos_to_altaz :: (Double, Double) -> (Double, Double)
dircos_to_altaz (l, m) 
  | (abs l < 1 && abs m  < 1) = (a,b)
  | otherwise = (0,0)
  where a = pi/2 - acos n 
        b = pi/2 - atan2 m l
        n = sqrt(1.0 - l*l - m*m)
hadec_to_altaz :: (Double, Double) -> Double -> (Double, Double)
hadec_to_altaz (ha, dec) lat 
  | (abs(lat)<pi/2) && (dec > -90 && dec < 90) =
    case () of
      () | sin(ha) < 0 -> (alt,a)
         | otherwise -> (alt,360-a)
  | otherwise = (0,0)
  where alt = asin(sin(dec)*sin(lat) + cos(dec)*cos(lat)*cos(ha))
        a = acos((sin(dec) - sin(alt)*sin(lat))/(cos(alt)*cos(lat)))

altaz_to_hadec :: (Double, Double) -> Double -> (Double, Double)
altaz_to_hadec (alt, az) lat = (acos ha, asin dec)
  where dec = sin(alt) * sin(lat) + cos(alt) * cos(lat) * cos(az)
        ha = (sin(alt) - sin(lat) * sin(dec))/(cos(lat) * cos(dec))

hadec_to_radec :: (Double, Double) -> Double -> (Double,Double)
hadec_to_radec (ha, dec) lst = (lst-ha,dec)

radec_to_hadec :: (Double, Double) -> Double -> (Double,Double)
radec_to_hadec (ra, dec) lst = (lst-ra,dec)

calculate_geometric_delay :: (Double, Double) -> (Double, Double, Double) -> Double
calculate_geometric_delay (l, m) (x, y, z)
  | (sqrt (l**2 + m**2 + n**2)) < 1.0 = (l*x + m*y + n*z)/3e8
  | otherwise = 0.0
  where n = sqrt(1.0 - l*l - m*m)
  
xyz_to_uvw :: (Double, Double, Double) -> Double -> (Double,Double,Double)
xyz_to_uvw (x, y, z) frequency = (u,v,w)
  where u = x / (3e8/frequency)
        v = y / (3e8/frequency)
        w = z / (3e8/frequency)

uvw_to_xyz :: (Double, Double, Double) -> Double -> (Double,Double,Double)
uvw_to_xyz (u, v, w) frequency = (x,y,z)
  where x = u * (3e8/frequency)
        y = v * (3e8/frequency)
        z = w * (3e8/frequency)
