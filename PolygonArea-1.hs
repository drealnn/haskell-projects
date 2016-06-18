module PolygonArea where

det :: (Double, Double) -> (Double, Double) -> Double
det (x1,y1) (x2,y2) = x1*y2 - x2*y1
 
partArea :: [(Double, Double)] -> Double
partArea [] = 0.0
partArea (x1:[]) = 0.0
partArea (x1:x2:xs) = det x1 x2 + (partArea (x2:xs))

computeArea :: [(Double,Double)] -> Double
computeArea [] = error "No points found"
computeArea (x:[]) = error "Needs at least two points"
computeArea (x:xs) = 0.5 * ( partArea (x:xs) + (det (last xs) x ) ) 
