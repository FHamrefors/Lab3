-- | The Tetris game (main module)
module Tetris where
import ConsoleGUI       -- cabal install ansi-terminal 
--import CodeWorldGUI     -- cabal install codeworld-api
import Shapes

--------------------------------------------------------------------------------
-- * The code that puts all the piece together

main = runGame tetrisGame

tetrisGame = Game { startGame     = startTetris,
                    stepGame      = stepTetris,
                    drawGame      = drawTetris,
                    gameInfo      = defaultGameInfo prop_Tetris,
                    tickDelay     = defaultDelay,
                    gameInvariant = prop_Tetris }

--------------------------------------------------------------------------------
-- * The various parts of the Tetris game implementation


-- | The state of the game
data Tetris = Tetris (Vector,Shape) Shape [Shape]
-- The state consists of three parts:
--   * The position and shape of the falling piece
--   * The well (the playing field), where the falling pieces pile up
--   * An infinite supply of random shapes

-- ** Positions and sizes

type Vector = (Int,Int)

-- | The size of the well
wellSize :: (Int,Int)
wellSize = (wellWidth,wellHeight)
wellWidth = 10
wellHeight = 20

-- | Starting position for falling pieces
startPosition :: Vector
startPosition = (wellWidth `div` 2 - 1, 0)

-- | Vector addition
vAdd :: Vector -> Vector -> Vector
(x1,y1) `vAdd` (x2,y2) = (x1+x2,y1+y2)

-- | Move the falling piece into position
place :: (Vector,Shape) -> Shape
place (v,s) = shiftShape v s

--B4
-- | An invariant that startTetris and stepTetris should uphold
prop_Tetris :: Tetris -> Bool
prop_Tetris (Tetris (_,s) w _) = (shapeSize w == wellSize) && (prop_Shape s)


--B5
-- | Add black walls around a shape
addWalls :: Shape -> Shape
addWalls s = S (addLeft 1(addRight 1 (addAbove s)))

addAbove :: Shape -> [Row]
addAbove s = [replicate n (Just Black)] ++ (rows s) ++ [replicate n (Just Black)]
  where n = fst(shapeSize s)

addRight :: Int -> [Row] -> [Row]
addRight n row = [(replicate n (Just Black)) ++ r | r <- row]

addLeft :: Int -> [Row] -> [Row]
addLeft n row = [r ++ (replicate n (Just Black)) | r <-row]


--B6
-- | Visualize the current game state. This is what the user will see
-- when playing the game.
drawTetris :: Tetris -> Shape
drawTetris (Tetris (v,p) w _) = addWalls (combine (shiftShape v p) w)


--B7
-- | The initial game state
startTetris :: [Double] -> Tetris
startTetris rs = Tetris (startPosition,shape1) (emptyShape wellSize) supply
  where
    shape1:supply = repeat (allShapes!!1) -- incomplete !!!

move :: Vector -> Tetris -> Tetris
move v1 (Tetris (v2,p) w s) = (Tetris((vAdd v1 v2),p) w s)

--B8
-- | React to input. The function returns 'Nothing' when it's game over,
-- and @'Just' (n,t)@, when the game continues in a new state @t@.
stepTetris :: Action -> Tetris -> Maybe (Int,Tetris)
stepTetris _ t = tick t

--tick :: Tetris -> Maybe (Int,Tetris)
--tick (Tetris (v,p) w s) = Just (0,t)
   --where t = (move (0,1) (Tetris (v,p) w s)) 
   
tick :: Tetris -> Maybe (Int,Tetris)
tick t
       | collision (move (0,1) t) = Just (0,t)
       | otherwise = Just (0, (move (0,1) t)) 
           


--C1
-- | Checks if the falling piece has collided with the walls or something in the well
collision :: Tetris -> Bool
collision (Tetris (v,p) w s) = shapeRight || shapeDown || shapeLeft || shapeOverlap
   where shapeRight   = (fst (shapeSize p) + fst v) >wellWidth
         shapeDown    = ((snd (shapeSize p) + snd v) >wellHeight) 
         shapeLeft    = ((fst v) <0)
         shapeOverlap = (overlaps (place (v,p)) w)
