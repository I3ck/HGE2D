module HGE2D.Datas where

import HGE2D.Types

import qualified Graphics.Rendering.OpenGL as GL

data RigidBody = RigidBody
    { rigidPos  :: RealPosition -- current position
    , rigidVel  :: Velocity     -- current velocity
    , rigidBB   :: BoundingBox  -- bounding box
    } deriving (Show, Read)


data BoundingBox = BoundingBox
    { bbMin     :: RealPosition -- lower left corner of bb
    , bbMax     :: RealPosition -- upper right corner of bb
    } deriving (Show, Read)

type WayPoint = BoundingBox

data TilePosition = TilePosition
    { tileX     :: Int -- position in number of tiles from left starting with 0
    , tileY     :: Int -- position in number of tiles from top starting with 0
    } deriving (Show, Read, Eq)


data RealPosition = RealPosition ---TODO rename to PixelPosition
    { realX     :: Pixel -- real position absolutely from left
    , realY     :: Pixel -- real position absolutely from right
    } deriving (Show, Read, Eq)


data Velocity = Velocity
    { velX      :: Double -- velocity in horizontal direction
    , velY      :: Double -- velocity in vertical direction
    } deriving (Show, Read)


--------------------------------------------------------------------------------

data RenderInstruction = RenderNothing
                       | RenderWithCamera GlPosX GlPosY GlScaleX GlScaleY RenderInstruction
                       | RenderText String
                       | RenderLineStrip GlShape GL.GLfloat
                       | RenderTriangle GlShape
                       | RenderLineLoop GlShape GL.GLfloat
                       | RenderScale GlScaleX GlScaleY
                       | RenderTranslate GlPosX GlPosY
                       | RenderRotate Double
                       | RenderColorize GlColorRGB
                       | RenderColorizeAlpha GlColorRGBA
                       | RenderPreserve [RenderInstruction]
                       | RenderMany [RenderInstruction]

