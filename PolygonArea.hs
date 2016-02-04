module PolygonArea where

det :: (Double, Double) -> (Double, Double) -> Double
det (x1,y1) (x2,y2) = x1*y2 - x2*y1
 
partArea :: [(Double, Double)] -> Double
partArea [] = 0.0
partArea (x1:[]) = 0.0
partArea (x1:x2:xs) = det x1 x2 + (partArea (x2:xs))

computeArea :: [(Double,Double)] -> Double
computeArea [] = 0.0
computeArea (x:[]) = 0.0
computeArea (x:xs) = 0.5 * ( partArea (x:xs) + (det x (last xs)) ) 
