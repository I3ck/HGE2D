-- |
-- Module      :  HGE2D.Datas
-- Copyright   :  (c) 2016 Martin Buck
-- License     :  see LICENSE
--
-- Containing data definitions used within HGE2D

module HGE2D.Datas where

import HGE2D.Types

import qualified Graphics.Rendering.OpenGL as GL

-- | A physical object with position, sizes, velocities, accelerations, mass, drag etc.
data PhysicalObject = PhysicalObject
    { physicalPos               :: RealPosition
    , physicalVel               :: Velocity
    , physicalAcc               :: Acceleration
    , physicalBB                :: BoundingBox
    , physicalRot               :: Radian
    , physicalRotSpeed          :: RotationSpeed ---TODO rename to ..Vel
    , physicalRotAcceleration   :: RotationAcceleration ---TODO rename to Acc
    , physicalMass              :: Mass
    , physicalDrag              :: Drag
    , physicalRotDrag           :: Drag ---TODO RotDrag?
    } deriving (Show, Read, Eq)

-- | A rigidbody defined by position, size and velocity
data RigidBody = RigidBody
    { rigidPos  :: RealPosition -- current position
    , rigidVel  :: Velocity     -- current velocity
    , rigidBB   :: BoundingBox  -- bounding box
    } deriving (Show, Read, Eq)

-- | A bounding box defined by two positions in space
data BoundingBox = BBEmpty
                 | BB
                    { bbMin     :: RealPosition -- lower left corner of bb
                    , bbMax     :: RealPosition -- upper right corner of bb
                    }
                 deriving (Show, Read, Eq)

-- | A position defined in number of tiles in x and y direction
data TilePosition = TilePosition
    { tileX     :: Int -- position in number of tiles from left starting with 0
    , tileY     :: Int -- position in number of tiles from top starting with 0
    } deriving (Show, Read, Eq)

--------------------------------------------------------------------------------

-- | The enginestate which defines multiple callbacks required by the engine to interact with the gamestate
data EngineState a = EngineState
    { click           :: PosX -> PosY -> a -> a -- how your game should change when clicked
    , mUp             :: PosX -> PosY -> a -> a -- how your game should change when mouse up happens
    , hover           :: PosX -> PosY -> a -> a -- how your game should change when hovered
    , drag            :: PosX -> PosY -> a -> a -- how your game should change when dragged
    , kDown           :: PosX -> PosY -> Char -> a -> a -- how your game should change when a keyDown happened
    , kUp             :: PosX -> PosY -> Char -> a -> a -- how your game should change when a keyUp happened
    , resize          :: (Width, Height) -> a -> a -- how to resize your game
    , getSize         :: a -> (Width, Height) -- how to get the size of your game
    , moveTime        :: Millisecond -> a -> a -- how your game should change over time
    , getTime         :: a -> Millisecond -- how to get the current time of your game
    , setTime         :: Millisecond -> a -> a -- how to set the time of your game
    , getTitle        :: a -> String -- how to get the title of your game
    , toGlInstr       :: a -> RenderInstruction -- how to receive a render instruction to display your game
    }

--------------------------------------------------------------------------------

-- | The instructions used to render the game
data RenderInstruction = RenderNothing                                                       -- do nothing
                       | RenderWithCamera GlPosX GlPosY GlScaleX GlScaleY RenderInstruction  -- render with a given camera view
                       | RenderText String                                                   -- render a string
                       | RenderPoints GlShape                                                -- render as points
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
