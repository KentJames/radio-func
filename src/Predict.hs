import Data.Complex
import Data.Array
import System.Random


predict_point_from_sources :: [Float] -> [Float] -> (Float,Float) -> Complex Float
predict_point_from_sources ls ms (u,v) =  foldl1 (+) $ map (\(l, m) ->
                                                               cos(2*pi * (u*l + v*m)) :+
                                                               sin(2*pi * (u*l + v*m))) $ zip ls ms

sample_uv_grid :: Int -> Float -> [Float] -> [Float] -> Array(Int, Int) (Complex Float)
sample_uv_grid size du ls ms =
  array((0,0),(size-1,size-1))
  [((x,y),predict_point_from_sources ls ms (du*fromIntegral(x - quot size 2),du*fromIntegral(y - quot size 2))) | x<-[0..size-1], y<-[0..size-1]]

generateArray :: Float -> Float -> IO [Float]
generateArray lowerb upperb =
  (getStdGen >>= return . randomRs (lowerb, upperb))

main :: IO()
main = do
  l <- generateArray 0.0 1.0
  m <- generateArray 0.0 1.0
  let ls = map (\x -> (x - 0.5)*0.1) $ take 100 $ l
  let ms = map (\x -> (x - 0.5)*0.1) $ take 100 $ m
  let point = predict_point_from_sources ls ms (3.4,5.6)
  print point
  let uvgrid = sample_uv_grid 5 5.0 ls ms
  print uvgrid
