module Mouse_handling (
    handler,
    find_cell_coords,
    max_field_height,
    max_field_width
    ) where

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Map
import Data.List
import System.Random
import Types

handler :: Event -> Game -> Game
handler (EventKey (MouseButton LeftButton) Down _ mouse) (Game state g_state g_field scr_size r_g f_size) =
    case state of
    MainMenu -> if (check_first_button mouse scr_size) then (Game ChooseMode g_state g_field scr_size r_g f_size)
                else if (check_second_button mouse scr_size) then (Game Settings g_state g_field scr_size r_g f_size)
                else (Game state g_state g_field scr_size r_g f_size)
    ChooseMode -> if (check_first_button mouse scr_size) then (Game ChooseMoveOrder g_state g_field scr_size r_g f_size)
                  else if (check_second_button mouse scr_size) then (Game HumanMove CrossMove (Data.Map.fromList
                    [((x, y), Empty) | x <- [1 .. (fst f_size)], y <- [1 .. (snd f_size)]]) scr_size r_g f_size)
                  else if (check_third_button mouse scr_size) then (Game MainMenu g_state g_field scr_size r_g f_size)
                  else (Game state g_state g_field scr_size r_g f_size)
    ChooseMoveOrder -> if (check_third_button mouse scr_size) then (Game ChooseMode g_state g_field scr_size r_g f_size)
                       else if (check_first_button mouse scr_size) then (Game HumanMove C_CrossMove (Data.Map.fromList
                        [((x, y), Empty) | x <- [1 .. (fst f_size)], y <- [1 .. (snd f_size)]]) scr_size r_g f_size)
                       else if (check_second_button mouse scr_size) then (Game ComputerMove C_ZeroMove
                        (Data.Map.fromList [((x, y), Empty) | x <- [1 .. (fst f_size)],
                        y <- [1 .. (snd f_size)]]) scr_size r_g f_size)
                       else (Game state g_state g_field scr_size r_g f_size)
    HumanMove -> if Data.Map.lookup (find_cell mouse f_size) g_field == Just Empty then
                     if g_state == C_CrossMove then (Game ComputerMove C_ZeroMove
                        (Data.Map.insert (find_cell mouse f_size) Cross g_field) scr_size r_g f_size)
                     else if g_state == CrossMove then (Game HumanMove ZeroMove
                        (Data.Map.insert (find_cell mouse f_size) Cross g_field) scr_size r_g f_size)
                     else (Game HumanMove CrossMove
                        (Data.Map.insert (find_cell mouse f_size) Zero g_field) scr_size r_g f_size)
                 else (Game state g_state g_field scr_size r_g f_size)
    GameOver -> if (check_back_to_menu_button mouse scr_size) then
                    (Game MainMenu None Data.Map.empty scr_size r_g f_size)
                else (Game state g_state g_field scr_size r_g f_size)
    Settings -> if (check_third_button mouse scr_size) then (Game MainMenu g_state g_field scr_size r_g f_size)
                else if (check_up1_button mouse scr_size) && (fst f_size) < (max_field_width scr_size) then
                    (Game Settings g_state g_field scr_size r_g ((fst f_size) + 1, snd f_size))
                else if (check_down1_button mouse scr_size) && (fst f_size) > 10 then
                    (Game Settings g_state g_field scr_size r_g ((fst f_size) - 1, snd f_size))
                else if (check_up2_button mouse scr_size) && (snd f_size) < (max_field_height scr_size) then
                    (Game Settings g_state g_field scr_size r_g (fst f_size, (snd f_size) + 1))
                else if (check_down2_button mouse scr_size) && (snd f_size) > 10 then
                    (Game Settings g_state g_field scr_size r_g (fst f_size, (snd f_size) - 1))
                else (Game state g_state g_field scr_size r_g f_size)
    _ -> (Game state g_state g_field scr_size r_g f_size)
handler _ something = something

check_first_button :: Point -> ScreenSize -> Bool
check_first_button (m1, m2) (w, h) = (m1 >= (- fromIntegral w / 7.68) && m1 <= ( fromIntegral w / 7.68)) &&
    (m2 >= (- fromIntegral h / 17.28) && m2 <= (fromIntegral h / 17.28))

check_second_button :: Point -> ScreenSize -> Bool
check_second_button (m1, m2) (w, h) = (m1 >= (- fromIntegral w / 7.68) && m1 <= ( fromIntegral w / 7.68)) &&
    (m2 >= (- fromIntegral h / 17.28 - fromIntegral h / 5.76) && m2 <= (fromIntegral h / 17.28 - fromIntegral h / 5.76))

check_third_button :: Point -> ScreenSize -> Bool
check_third_button (m1, m2) (w, h) = (m1 >= (- fromIntegral w / 7.68) && m1 <= ( fromIntegral w / 7.68)) &&
    (m2 >= (- fromIntegral h / 17.28 - fromIntegral h / 2.88) && m2 <= (fromIntegral h / 17.28 - fromIntegral h / 2.88))

check_back_to_menu_button :: Point -> ScreenSize -> Bool
check_back_to_menu_button (m1, m2) (w, h) = (m1 >= (- fromIntegral w / 7.68) && m1 <= ( fromIntegral w / 7.68)) &&
    (m2 >= (- fromIntegral h / 17.28 - fromIntegral h / 2.4) && m2 <= (fromIntegral h / 17.28 - fromIntegral h / 2.4))

check_up1_button :: Point ->ScreenSize -> Bool
check_up1_button (m1, m2) (w, h) = (m1 >= (-fromIntegral w / 30.72 + fromIntegral w / 5.12) &&
    m1 <= (fromIntegral w / 30.72 + fromIntegral w / 5.12)) &&
    (m2 >= (-fromIntegral h / 34.56) && m2 <= (fromIntegral h / 34.56))

check_down1_button :: Point ->ScreenSize -> Bool
check_down1_button (m1, m2) (w, h) = (m1 >= (-fromIntegral w / 30.72 + fromIntegral w / 3.41) &&
    m1 <= (fromIntegral w / 30.72 + fromIntegral w / 3.41)) &&
    (m2 >= (-fromIntegral h / 34.56) && m2 <= (fromIntegral h / 34.56))

check_up2_button :: Point ->ScreenSize -> Bool
check_up2_button (m1, m2) (w, h) = (m1 >= (-fromIntegral w / 30.72 + fromIntegral w / 5.12) &&
    m1 <= (fromIntegral w / 30.72 + fromIntegral w / 5.12)) &&
    (m2 >= (-fromIntegral h / 34.56 - fromIntegral h / 5.76) && m2 <= (fromIntegral h / 34.56 - fromIntegral h / 5.76))

check_down2_button :: Point ->ScreenSize -> Bool
check_down2_button (m1, m2) (w, h) = (m1 >= (-fromIntegral w / 30.72 + fromIntegral w / 3.41) &&
    m1 <= (fromIntegral w / 30.72 + fromIntegral w / 3.41)) &&
    (m2 >= (-fromIntegral h / 34.56 - fromIntegral h / 5.76) && m2 <= (fromIntegral h / 34.56 - fromIntegral h / 5.76))

find_cell :: Point -> FieldSize -> (Int, Int)
find_cell (m1, m2) (field_width, field_height) =
    if (round (m1 + fromIntegral field_width * 30 / 2)) `mod` 30 + 1 /= 0 &&
    (round (m2 + fromIntegral field_height * 30 / 2)) `mod` 30 + 1 /= 0 then
    ((round (m1 + fromIntegral field_width * 30 / 2)) `div` 30 + 1,
    (round (m2 + fromIntegral field_height * 30 / 2)) `div` 30 + 1)
                     else (0, 0)

find_cell_coords :: (Int, Int) -> FieldSize -> Point
find_cell_coords (f_w, f_h) (field_width, field_height) =
    (fromIntegral ((f_w - 1) * 30 + 15) - fromIntegral field_width * 30 / 2,
    fromIntegral ((f_h - 1) * 30 + 15) - fromIntegral field_height * 30 / 2)

max_field_width :: ScreenSize -> Int
max_field_width (w, h) = max (5 * (w `div` 150)) 10

max_field_height :: ScreenSize -> Int
max_field_height (w, h) = max (5 * (h `div` 216)) 10