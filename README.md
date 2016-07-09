# HGE2D
A 2D game engine written in and for Haskell
## Version 0.1.5.1

## Install
`git clone https://github.com/I3ck/HGE2D.git`  
`cd HGE2D`  
`cabal install` (to install the lib)  
`cabal build` (to build the examples)

## Usage
You have the state of your game. It should keep all the data you need to store
for interactions with your game and rendering it.  
A very basic example :  
```haskell
data GameState = GameState
    { gsSize :: (Double, Double) -- size of the window
    , time   :: Millisecond      -- current time of your game's state
    , player :: Player           -- your definition for a player
    , world  :: World            -- your definition for the world    
    }
```
Note that none of these parameters are actually required. Just use what you'll need later on. A game which isn't dynamic won't have to store a time etc.  
As next step you'll have to define the `EngineState` for your `GameState` :  
```haskell
es = EngineState
    { getTitle = myGetTitle
    , getW = myGetW
    , getH = myGetH
    , getTime = myGetTime
    , setTime = mySetTime
    , moveTime = myMoveTime
    , click = myClick
    , hover = myHover
    , drag = myDrag
    , resize = myResize
    , getSize = myGetSize
    , toGlInstr = myToGlInstr
    } :: EngineState GameState
```
`EngineState` serves as collection of all the required functions for the game engine.  
You'll have to define all of these, the definitions are as follows (see examples for implementations) :   
```haskell
data EngineState a = EngineState
    { click           :: PosX -> PosY -> a -> a -- how your game should change when clicked
    , hover           :: PosX -> PosY -> a -> a -- how your game should change when hovered
    , drag            :: PosX -> PosY -> a -> a -- how your game should change when dragged
    , resize          :: (Width, Height) -> a -> a -- how to resize your game
    , getSize         :: a -> (Width, Height) -- how to get the size of your game
    , moveTime        :: Millisecond -> a -> a -- how your game should change over time
    , getTime         :: a -> Millisecond -- how to get the current time of your game
    , setTime         :: Millisecond -> a -> a -- how to set the time of your game
    , getTitle        :: a -> String -- how to get the title of your game
    , toGlInstr       :: a -> RenderInstruction -- how to receive a render instruction to display your game
    }
```

### toGlInstr
To actually render your game, `HGE2D` needs to know its RenderInstruction.  
```haskell
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
                       | RenderPreserve RenderInstruction
                       | RenderMany [RenderInstruction]
```
`RenderInstruction` can be used to define pretty much any scene by combining the instructions.  
`RenderNothing` obviously does nothing and can be used when e.g. shapes shall be invisible / not rendered.  
`RenderText` is also obvious, since it will simply render a string.  
`RenderMany` can be used to combine multiple instructions. Your `GameState` will pretty likely be defined as a `RenderMany [player, map, ...]`.  
Scaling, translating and rotating will affect all the shapes to come. To scope them, use `RenderPreserve`. In combination with `RenderMany` :  
`RenderPreserve $ RenderMany [RenderTranslate 10 10, RenderRotate 0.1, player]`.  
See [Shapes.hs](src/HGE2D/Shapes.hs) for functions which will generate some basic shapes. It's also pretty straight forward to define your very own primitives in a similar manner.  
For more information please take a look at the examples. These will also always be up-to-date since they're compiled on every change.

## Examples

[Simple "Hello world", showing draw methods and how to setup the state](src/examples/Example1.hs)  

[Example showing how to react to mouse actions. Also first example showing how to alter the game state](src/examples/Example2.hs)  

[Example showing how to add dynamic motions dependend on the time](src/examples/Example3.hs)  

[Example showing how to nest several RenderInstructions to create a single instruction by combination](src/examples/Example4.hs)

## Contribute

Feel free to open an issue whenever you find a bug or are missing a feature.  
Also new shapes and named colors are always really helpful and should be pretty easy to contribute.  
Also I'd love to hear if you used this engine to write your own game.
