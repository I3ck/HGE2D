module HGE2D.Collision where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes

--------------------------------------------------------------------------------

doCollide :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
doCollide hasBB1 hasBB2 = 2.0 * xcenterthis - xcenterother < (xsizethis + xsizeother) && 2.0 * ycenterthis - ycenterother < (ysizethis + ysizeother)
    where
      bb1 = getBB hasBB1
      bb2 = getBB hasBB2
      minthis = bbMin bb1
      maxthis = bbMax bb1
      minother = bbMin bb2
      maxother = bbMax bb2
      xsizethis = abs $ (fst minthis) - (fst maxthis)
      ysizethis = abs $ (snd minthis) - (snd maxthis)
      xsizeother = abs $ (fst minother) - (fst maxother)
      ysizeother = abs $ (snd minother) - (snd maxother)
      xcenterthis = (fst minthis) + (fst maxthis) / 2.0
      ycenterthis = (snd minthis) + (snd maxthis) / 2.0
      xcenterother = (fst minother) + (fst maxother) / 2.0
      ycenterother = (snd minother) + (snd maxother) / 2.0

--------------------------------------------------------------------------------

doContain :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
doContain hasBB1 hasBB2 =   (isInside hasBB1 hasBB2)
                         || (isInside hasBB2 hasBB1)

--------------------------------------------------------------------------------

isInside :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
isInside hasBBIn hasBBOut =    (bbInTop    > bbOutTop)
                            && (bbInBottom < bbOutBottom)
                            && (bbInLeft   > bbOutLeft)
                            && (bbInRight  < bbOutRight)
  where
      bbInTop     = snd $ bbMin bbIn
      bbInRight   = fst $ bbMax bbIn
      bbInBottom  = snd $ bbMax bbIn
      bbInLeft    = fst $ bbMin bbIn

      bbOutTop    = snd $ bbMin bbOut
      bbOutRight  = fst $ bbMax bbOut
      bbOutBottom = snd $ bbMax bbOut
      bbOutLeft   = fst $ bbMin bbOut

      bbIn        = getBB hasBBIn
      bbOut       = getBB hasBBOut

--------------------------------------------------------------------------------

isInsideRP :: (HasBoundingBox a) => RealPosition -> a -> Bool
isInsideRP pos hasBB =  (posX > bbLeft)
                     && (posX < bbRight)
                     && (posY > bbTop)
                     && (posY < bbBottom)
  where
    posX     = fst pos
    posY     = snd pos
    bbTop    = snd $ bbMin bb
    bbRight  = fst $ bbMax bb
    bbBottom = snd $ bbMax bb
    bbLeft   = fst $ bbMin bb
    bb       = getBB hasBB
