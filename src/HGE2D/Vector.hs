-- |
-- Module      :  Engine.Data.Vector
-- Copyright   :  (c) 2017 Giorgio Susanna
-- License     :  see LICENSE
--
-- Module containing a vectors generalization.
-- NOTE: It relies on OpenGL Vector2 and Vector3 definition.

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE UndecidableInstances   #-}

module HGE2D.Vector
( module V
, Vec1(..)
, Vec2(..)
, Vec3(..)
, (+*)
, (.*)
, (!*)
, len
, sqrLen
, nor
, dst
, sqrDst
) where

import Graphics.Rendering.OpenGL.GL.Tensor as V
import Control.Applicative (liftA2)

-- | == Vector Classes

class (Num b, Applicative a, Foldable a, Functor a) => Vec1 a b where
    -- | Generate a Vector with 1 argument.
    vec1 :: b -> a b
    getX :: a b -> b

class Vec1 a b => Vec2 a b where
    -- | Generate a Vector with 2 arguments.
    vec2 :: b -> b -> a b
    getY :: a b -> b

class Vec2 a b => Vec3 a b where
    -- | Generate a Vector with 3 arguments.
    vec3 :: b -> b -> b -> a b
    getZ :: a b -> b


--------------------------------------------------------------------------------


-- | == Vector2 Instances

-- | Instance of Vec1 for Vector2 and Num
instance Num a => Vec1 (Vector2) a where
    vec1 = pure
    getX (Vector2 x _) = x

-- | Instance of Vec2 for Vector2 and Num
instance Num a => Vec2 (Vector2) a where
    -- | ==== __Example__
    --
    -- >>> vec2 1 2 :: Vector2 Int
    -- Vector2 1 2
    vec2 = Vector2
    getY (Vector2 _ y) = y


--------------------------------------------------------------------------------


-- | == Vector3 Instances

-- | Instance of Vec1 for Vector3 and Num
instance Num a => Vec1 (Vector3) a where
    -- | ==== __Example__
    --
    -- >>> vec1 1 :: Vector3 Int
    -- Vector3 1 1
    vec1 = pure
    getX (Vector3 x _ _) = x

-- | Instance of Vec2 for Vector3 and Num
instance Num a => Vec2 (Vector3) a where
    -- | ==== __Example__
    --
    -- >>> vec2 3 2 :: Vector3 Int
    -- Vector3 6 6 6
    vec2 x y = pure $ (*) x y
    getY (Vector3 _ y _) = y

-- | Instance of Vec3 for Vector3 and Num
instance Num a => Vec3 (Vector3) a where
    -- | ==== __Example__
    --
    -- >>> vec3 1 2 3 :: Vector3 Int
    -- Vector3 1 2 3
    vec3 = Vector3
    getZ (Vector3 _ _ z) = z


--------------------------------------------------------------------------------


-- | Instance of Num for Vec
instance (Vec1 v a) => Num (v a) where

    -- | Sum corresponding components of a Vector2.
    --
    -- ==== __ Examples __
    --
    -- @ >>> Vector2 10 4 + Vector2 5 2
    -- @ Vector2 15 6
    (+) = liftA2 (+)

    -- | Subtract corresponding components of a Vector2.
    --
    -- ==== __ Examples __
    --
    -- @ >>> Vector2 10 4 - Vector2 5 2
    -- @ Vector2 5 2
    (-) = liftA2 (-)

    -- | Multiply corresponding components of a Vector2.
    --
    -- ==== __ Examples __
    --
    -- @ >>> Vector2 10 4 * Vector2 5 2
    -- @ Vector2 50 8
    (*) = liftA2 (*)

    -- | Evaluate the absolute value of each of its component.
    --
    -- ==== __ Examples __
    --
    -- @ >>> abs $ Vector2 (-4) 3
    -- @ Vector2 4 3
    abs     = fmap abs

    -- | Evaluate the signum of each of its components.
    --
    -- ==== __Examples__
    --
    -- @ >>> signum (Vector2 (-33) (1))
    -- @ Vector2 (-1) (1)
    signum  = fmap signum

    -- | Defines a Vector2 with both variable equals.
    --
    -- ==== __Examples__
    --
    -- @ >>> fromInteger 2 :: Vector2 Double
    -- @ Vector2 2.0 2.0
    fromInteger = pure . fromInteger

--------------------------------------------------------------------------------


-- | Instance of Fractional for Vec
instance (Fractional a, Vec1 v a) => Fractional (v a) where

    -- | Divide corresponding components of a Vector2.
    --
    -- ==== __ Examples __
    --
    -- @ >>> Vector2 10 4 / Vector2 5 2
    -- @ Vector2 2.0 2.0
    (/) = liftA2 (/)

--------------------------------------------------------------------------------


-- | == Utility Function


infixl 7 !*, +*, .*

-- | Cross product of two Vectors.
-- Given a vector |x, y| and a vector |x', y'|,
-- the cross product is defined as follow:
--
-- *@x * y' - x' * y
--
-- ==== __Examples__
--
-- @ >>> vec2 3 1 +* vec2 1 1
-- @ 2
(+*) :: (Num a, Vec2 v a) => v a -> v a -> a
(+*) v1 v2 = xy - yx
    where
        xy = getX v1 * getY v2
        yx = getY v1 * getX v2


-- | Dot product or scalar product of two Vectors.
-- Given two vectors: |x, y, z| and |x', y', z'|,
-- the dot product is defined as follow:
--
-- *@x * x' + y * y' + z * z'
--
-- ==== __Examples__
--
-- @ >>> vec2 3 1 .* vec2 4 1
-- @ 13
(.*) :: (Num a, Vec1 v a) => v a -> v a -> a
(.*) v1 v2 = sum $ liftA2 (*) v1 v2

-- | Scale a Vector
--
-- ==== __Examples__
--
-- @ >>> vec2 1 2 !* 3.0
-- @ Vector2 3 6
(!*) :: (Num a, Vec1 v a) => v a -> a -> v a
(!*) v f = fmap (*f) v

-- | Evaluate a Vector's magnitude (length).
--
-- ==== __Examples__
--
-- @ >>> magnitude (vec2 3 4 :: Vector2 Float)
-- @ 5.0
len    :: (Floating a, Vec1 v a) => v a -> a
len    = sqrt . sqrLen

-- | Evaluate a Vector's squared magnitude (length).
--
-- ==== __Examples__
--
-- @ >>> sqrMagnitude (vec2 3 4 :: Vector2 Float)
-- @ 25.0
sqrLen :: (Floating a, Vec1 v a) => v a -> a
sqrLen = sum . fmap (**2)

-- | Normalize a Vector.
--
-- ==== __Examples__
--
-- @ >>> normalize (Vector2 3 1)
-- @ Vector2 0.9486832980505138 0.31622776601683794
nor   :: (Floating a, Vec1 v a) => v a -> v a
nor v  = fmap (/ len v) v

-- | Evaluate the distance between 2 Vectors.
--
-- ==== __Examples__
--
-- @ >>> dst (vec2 4 5 :: Vector2 Double) (vec2 1 1 :: Vector2 Double)
-- @ 5.0
dst        :: (Floating a, Vec1 v a) => v a -> v a -> a
dst v      = len . liftA2 (-) v

-- | Evaluate the squared distance between 2 Vectors.
--
-- ==== __Examples__
--
-- @ >>> sqrDst (vec2 4 5 :: Vector2 Double) (vec2 1 1 :: Vector2 Double)
-- @ 25.0
sqrDst     :: (Floating a, Vec1 v a) => v a -> v a -> a
sqrDst v   = sqrLen . liftA2 (-) v
