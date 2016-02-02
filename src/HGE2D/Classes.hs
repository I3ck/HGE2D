{-# LANGUAGE ConstraintKinds #-}
module HGE2D.Classes where

import HGE2D.Types
import HGE2D.Render

--------------------------------------------------------------------------------

class MouseInteract a where
    hover, click, drag :: Double -> Double -> a -> a

--------------------------------------------------------------------------------

class Resizeable a where ---TODO rename
    resize :: Double -> Double -> a -> a ---TODO (Double, Double)
    getSize :: a -> (Double, Double)

--------------------------------------------------------------------------------

class HasTime a where
    getTime    :: a -> Millisecond
    setTime    :: Millisecond -> a -> a
    moveInTime :: Millisecond -> a -> a

--------------------------------------------------------------------------------

class HasSize a where
    getW, getH :: a -> Double

--------------------------------------------------------------------------------

class HasTitle a where
    getTitle :: a -> String

--------------------------------------------------------------------------------

type EngineState a = 
    ( MouseInteract a
    , GlInstructable a
    , Resizeable a
    , HasTime a
    , HasSize a
    , HasTitle a
    )

--------------------------------------------------------------------------------
