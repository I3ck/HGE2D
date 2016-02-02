{-# LANGUAGE ConstraintKinds #-}
module HGE2D.Classes where

import HGE2D.Types
import HGE2D.Render

--------------------------------------------------------------------------------

class Dynamic a where
    applyTime :: Millisecond -> a -> a

--------------------------------------------------------------------------------

class MouseInteract a where
    hover, click, drag :: Double -> Double -> a -> a

--------------------------------------------------------------------------------

class Resizeable a where ---TODO rename
    resize :: Double -> Double -> a -> a ---TODO (Double, Double)
    getSize :: a -> (Double, Double)

--------------------------------------------------------------------------------

class HasTime a where ---TODO combine with Dynamic
    getTime :: a -> Int
    setTime :: Int -> a -> a
    moveInTime :: Int -> a -> a ---TODO remove, see notes in instance

--------------------------------------------------------------------------------

class HasSize a where
    getW :: a -> Double
    getH :: a -> Double

--------------------------------------------------------------------------------

class HasTitle a where
    getTitle :: a -> String

--------------------------------------------------------------------------------

type EngineState a = 
    ( MouseInteract a
    , Dynamic a
    , GlInstructable a
    , Resizeable a
    , HasTime a
    , HasSize a
    , HasTitle a
    )

--------------------------------------------------------------------------------
