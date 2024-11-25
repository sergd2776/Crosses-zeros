module Main where
import Graphics.Gloss.Interface.Environment
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Map
import Data.List
import System.Random

import Types
import Picture_rendering
import Mouse_handling
import Engine

main :: IO ()
main = do
    gss <- getScreenSize
    rand <- getStdGen
    play FullScreen white 90 (Game MainMenu None Data.Map.empty gss rand (15, 15)) renderer handler updater

