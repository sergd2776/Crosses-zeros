module Types (
    Field,
    FieldRatio,
    ScreenSize,
    FieldSize,
    CurrentState(
                MainMenu,
                ChooseMoveOrder,
                ChooseMode,
                HumanMove,
                ComputerMove,
                GameOver,
                Settings),
    CellState (Empty,
              Cross,
              Zero),
    GameState (
              None,
              ZeroMove,
              CrossMove,
              Draw,
              C_ZeroMove,
              C_CrossMove),
    Game (Game)
    ) where

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Map
import Data.List
import System.Random

type Field = Map (Int, Int) CellState
type FieldRatio = Map (Int, Int) Int
type ScreenSize =  (Int, Int)
type FieldSize = (Int, Int)

data CurrentState = MainMenu
                    | ChooseMoveOrder
                    | ChooseMode
                    | HumanMove
                    | ComputerMove
                    | GameOver
                    | Settings

data CellState = Empty
                | Cross
                | Zero deriving (Eq, Show)
data GameState = None
                | ZeroMove
                | CrossMove
                | Draw
                | C_ZeroMove
                | C_CrossMove deriving Eq

data Game = Game
    {
        flag :: CurrentState,
        game_state :: GameState,
        game_field :: Field,
        screen_res :: (Int, Int),
        rand_gen :: StdGen,
        field_size :: FieldSize
    }
