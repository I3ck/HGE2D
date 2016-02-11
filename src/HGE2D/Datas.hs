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

data RenderInstruction = RenderNothing                                                       -- do nothing
                       | RenderWithCamera GlPosX GlPosY GlScaleX GlScaleY RenderInstruction  -- render with a given camera view
                       | RenderText String                                                   -- render a string
                       | RenderLineStrip GlShape GL.GLfloat                                  -- render as line strip
                       | RenderTriangle GlShape                                              -- render as triangles / faces
                       | RenderLineLoop GlShape GL.GLfloat                                   -- render as line which connects first and last
                       | RenderScale GlScaleX GlScaleY                                       -- change scale
                       | RenderTranslate GlPosX GlPosY                                       -- translate / move following instructions
                       | RenderRotate Double                                                 -- rotate following instructions
                       | RenderColorize GlColorRGB                                           -- colorize following instructions
                       | RenderColorizeAlpha GlColorRGBA                                     -- colorize following instructions with alpha setting
                       | RenderPreserve RenderInstruction                                    -- render instruction while preserving rotation / translation
                       | RenderMany [RenderInstruction]                                      -- render multiple other instructions

