module Interferometer() where
import Geometry
import Data.List

antenna_coords_uvw :: [Double] -> [Double] -> [Double] -> Double -> [(Double, Double, Double)]
antenna_coords_uvw x y z freq = map (\xyz -> xyz_to_uvw xyz freq) ziplist
  where ziplist = zip3 x y z

