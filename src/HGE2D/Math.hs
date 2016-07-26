-- |
-- Module      :  HGE2D.Math
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing basic math functions

module HGE2D.Math where

---TODO return maybe instead of 0 0

-- | Calculate the two minima of the quadratic equation
quadraticEquation :: Double -> Double -> Double -> (Double, Double)
quadraticEquation a b c = if d < 0 then (0, 0) else (x, y)
                        where
                          x = e + sqrt d / (2 * a)
                          y = e - sqrt d / (2 * a)
                          d = b * b - 4 * a * c
                          e = - b / (2 * a)
