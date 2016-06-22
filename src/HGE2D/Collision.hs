module HGE2D.Collision where

import HGE2D.Types
import HGE2D.Datas
import HGE2D.Classes

--------------------------------------------------------------------------------

doCollide :: (HasBoundingBox a, HasBoundingBox b) => a -> b -> Bool
doCollide hasBB1 hasBB2 = not $    (bb2Left   > bb1Right)
                                || (bb2Right  < bb1Left)
                                || (bb2Top    > bb1Bottom)
                                || (bb2Bottom < bb1Top)
  where
    bb1Top    = snd $ bbMin bb1
    bb1Right  = fst $ bbMax bb1
    bb1Bottom = snd $ bbMax bb1
    bb1Left   = fst $ bbMin bb1

    bb2Top    = snd $ bbMin bb2
    bb2Right  = fst $ bbMax bb2
    bb2Bottom = snd $ bbMax bb2
    bb2Left   = fst $ bbMin bb2

    bb1 = getBB hasBB1
    bb2 = getBB hasBB2

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
