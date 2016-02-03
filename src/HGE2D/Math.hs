module HGE2D.Math where

---TODO proper error case?
quadraticEquation :: Double -> Double -> Double -> (Double, Double)
quadraticEquation a b c = if d < 0 then (0, 0) else (x, y)
                        where
                          x = e + sqrt d / (2 * a)
                          y = e - sqrt d / (2 * a)
                          d = b * b - 4 * a * c
                          e = - b / (2 * a)
