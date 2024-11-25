module Picture_rendering (
    renderer
    ) where

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Map
import Data.List
import System.Random
import Types
import Mouse_handling
import Monitoring

renderer :: Game -> Picture
renderer (Game state g_state f scr_size _ f_size) = case state of
    MainMenu -> (main_menu_pic scr_size)
    ChooseMode -> (choose_mode_pic scr_size)
    ChooseMoveOrder -> (choose_move_order_pic scr_size)
    HumanMove -> (game_field_pic scr_size f_size f)
    ComputerMove -> (game_field_pic scr_size f_size f)
    GameOver -> (game_over_pic scr_size f_size f g_state)
    Settings -> (settings_pic scr_size f_size)

zero_pic :: Point -> Picture
zero_pic (w, h) = translate w h (color red (circle 10))

cross_pic :: Point -> Picture
cross_pic (w, h) = Pictures [color azure (line [(w - 10, h - 10), (w + 10, h + 10)]),
                            color azure (line [(w + 10, h - 10), (w - 10, h + 10)])]

blank_white_pic :: ScreenSize -> Picture
blank_white_pic (w, h) = translate 0 0 (color white (polygon (rectanglePath (fromIntegral w) (fromIntegral h))))

net_pic :: ScreenSize -> Color -> Picture
net_pic (w, h) clr = Pictures (
    [color clr (line [(fromIntegral (x - w `div` 2), fromIntegral (-h `div` 2)), (fromIntegral (x - w `div` 2),
        fromIntegral (h `div` 2))])
    | x <- [w `mod` 30 `div` 2, w `mod` 30 `div` 2 + 30 .. fromIntegral w]]
    ++ [color clr (line [(fromIntegral (-w `div` 2), fromIntegral (x - h `div` 2)), (fromIntegral (w `div` 2),
        fromIntegral (x - h `div` 2))])
    | x <- [h `mod` 30 `div` 2, h `mod` 30 `div` 2 + 30 .. fromIntegral h]])

main_menu_pic :: ScreenSize -> Picture
main_menu_pic (w, h) = Pictures (
    [blank_white_pic (w, h),
    net_pic (w, h) (greyN 0.96),
    translate 0 (fromIntegral h / 2.88) (color orange (polygon (rectanglePath (fromIntegral w / 2.56)
        (fromIntegral h / 5.76)))),
    translate (-fromIntegral w / 8.53) (fromIntegral h / 3.2) (color black (scale (fromIntegral w / 3072)
        (fromIntegral h / 1728) (text "Main menu"))),
    translate 0 0 (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84) (fromIntegral h / 8.64)))),
    translate 0 (-fromIntegral h / 5.76) (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84)
        (fromIntegral h / 8.64)))),
    -- translate 0 (-fromIntegral h / 2.88) (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84)
    --    (fromIntegral h / 8.64)))),
    translate (-fromIntegral w / 61.44) (-fromIntegral h / 57.6) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Play"))),
    translate (-fromIntegral w / 30.72) (-fromIntegral h / 5.24) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Settings")))])
    -- translate (-fromIntegral w / 61.44) (-fromIntegral h / 2.74) (color black (scale (fromIntegral w / 6148)
    --    (fromIntegral h / 3456) (text "Exit")))])

settings_pic :: ScreenSize -> FieldSize -> Picture
settings_pic (w, h) (field_width, field_height) = Pictures (
    [blank_white_pic (w, h),
    net_pic (w, h) (greyN 0.96),
    translate 0 (fromIntegral h / 2.88) (color orange (polygon (rectanglePath (fromIntegral w / 2.56)
        (fromIntegral h / 5.76)))),
    translate (-fromIntegral w / 12.8) (fromIntegral h / 3.2) (color black (scale (fromIntegral w / 3072)
        (fromIntegral h / 1728) (text "Settings"))),
    translate 0 (-fromIntegral h / 2.88) (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84)
        (fromIntegral h / 8.64)))),
    translate (fromIntegral w / 5.12) 0 (color (state_green_button_width (w, h) field_width)
        (polygon (rectanglePath (fromIntegral w / 15.36) (fromIntegral h / 17.28)))),
    translate (fromIntegral w / 3.41) 0 (color (state_red_button_width (w, h) field_width)
        (polygon (rectanglePath (fromIntegral w / 15.36) (fromIntegral h / 17.28)))),
    translate (fromIntegral w / 5.12) (-fromIntegral h / 5.76)
        (color (state_green_button_height (w, h) field_height)
        (polygon (rectanglePath (fromIntegral w / 15.36) (fromIntegral h / 17.28)))),
    translate (fromIntegral w / 3.41) (-fromIntegral h / 5.76)
        (color (state_red_button_height (w, h) field_height)
        (polygon (rectanglePath (fromIntegral w / 15.36) (fromIntegral h / 17.28)))),
    translate (-fromIntegral w / 3.41) (-fromIntegral h / 57.6) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Field width (cells)"))),
    translate (-fromIntegral w / 3.41) (-fromIntegral h / 5.24) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Field height (cells)"))),
    translate (fromIntegral w / 4.33) (-fromIntegral h / 57.6) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text (show field_width)))),
    translate (fromIntegral w / 4.33) (-fromIntegral h / 5.24) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text (show field_height)))),
    translate (fromIntegral w / 5.49) (-fromIntegral h / 57.6) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Up"))),
    translate (fromIntegral w / 3.75) (-fromIntegral h / 57.6) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Down"))),
    translate (fromIntegral w / 5.49) (-fromIntegral h / 5.24) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Up"))),
    translate (fromIntegral w / 3.75) (-fromIntegral h / 5.24) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Down"))),
    translate (-fromIntegral w / 51.2) (-fromIntegral h / 2.74) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Back")))])

choose_mode_pic :: ScreenSize -> Picture
choose_mode_pic (w, h) = Pictures (
    [blank_white_pic (w, h),
    net_pic (w, h) (greyN 0.96),
    translate 0 (fromIntegral h / 2.88) (color orange (polygon (rectanglePath (fromIntegral w / 2.56)
        (fromIntegral h / 5.76)))),
    translate (-fromIntegral w / 6.144) (fromIntegral h / 3.2) (color black (scale (fromIntegral w / 4096)
        (fromIntegral h / 2304) (text "Choose game mode:"))),
    translate 0 0 (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84) (fromIntegral h / 8.64)))),
    translate 0 (-fromIntegral h / 5.76) (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84)
        (fromIntegral h / 8.64)))),
    translate 0 (-fromIntegral h / 2.88) (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84)
        (fromIntegral h / 8.64)))),
    translate (-fromIntegral w / 8.78) (-fromIntegral h / 57.6) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Play with a computer"))),
    translate (-fromIntegral w / 8.78) (-fromIntegral h / 5.24) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Play with your friend"))),
    translate (-fromIntegral w / 51.2) (-fromIntegral h / 2.74) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Back")))])

choose_move_order_pic :: ScreenSize -> Picture
choose_move_order_pic (w, h) = Pictures (
    [blank_white_pic (w, h),
    net_pic (w, h) (greyN 0.96),
    translate 0 (fromIntegral h / 2.88) (color orange (polygon (rectanglePath (fromIntegral w / 2.56)
        (fromIntegral h / 5.76)))),
    translate (-fromIntegral w / 6.144) (fromIntegral h / 3.2) (color black (scale (fromIntegral w / 4096)
        (fromIntegral h / 2304) (text "Choose move order:"))),
    translate 0 0 (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84) (fromIntegral h / 8.64)))),
    translate 0 (-fromIntegral h / 5.76) (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84)
        (fromIntegral h / 8.64)))),
    translate 0 (-fromIntegral h / 2.88) (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84)
        (fromIntegral h / 8.64)))),
    translate (-fromIntegral w / 51.2) (-fromIntegral h / 57.6) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "First"))),
    translate (-fromIntegral w / 30.72) (-fromIntegral h / 5.24) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Second"))),
    translate (-fromIntegral w / 51.2) (-fromIntegral h / 2.74) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Back")))])

game_field_pic :: ScreenSize -> FieldSize -> Field -> Picture
game_field_pic (w, h) (f_width, f_height) f = Pictures (
    [blank_white_pic (w, h),
    net_pic (w, h) (greyN 0.96),
    (color white (polygon (rectanglePath (fromIntegral w / 51.2 * fromIntegral f_width)
        (fromIntegral w / 51.2 * fromIntegral f_height)))),
    net_pic (30 * f_width, 30 * f_height) black,
    crosses_zeros_pic f (f_width, f_height),
    win_line_pic (check_win f (1, 1) (f_width, f_height)) (f_width, f_height)])

crosses_zeros_pic :: Field -> FieldSize -> Picture
crosses_zeros_pic curr_field (field_width, field_height) = Pictures ([one_cell_pic x y | x <- [1 .. field_width],
    y <- [1 .. field_height]]) where
                        one_cell_pic x y = case Data.Map.lookup (x, y) curr_field of
                            Just Cross -> cross_pic (find_cell_coords (x, y) (field_width, field_height))
                            Just Zero -> zero_pic (find_cell_coords (x, y) (field_width, field_height))
                            Just Empty -> Blank

win_line_pic :: ((Int, Int), (Int, Int)) -> FieldSize -> Picture
win_line_pic ((x1, y1), (x2, y2)) f_size = if x1 == 0 then Blank
    else if y1 == y2 then Pictures(
    [color black (line [((fst (find_cell_coords (x1, y1) f_size)) - 10, (snd (find_cell_coords (x1, y1) f_size))),
        ((fst (find_cell_coords (x2, y2) f_size)) + 10, (snd (find_cell_coords (x2, y2) f_size)))]),
    color black (line [((fst (find_cell_coords (x1, y1) f_size)) - 10, (snd (find_cell_coords (x1, y1) f_size)) + 1),
        ((fst (find_cell_coords (x2, y2) f_size)) + 10, (snd (find_cell_coords (x2, y2) f_size)) + 1)]),
    color black (line [((fst (find_cell_coords (x1, y1) f_size)) - 10, (snd (find_cell_coords (x1, y1) f_size)) - 1),
        ((fst (find_cell_coords (x2, y2) f_size)) + 10, (snd (find_cell_coords (x2, y2) f_size)) - 1)])])
    else if x1 == x2 then Pictures (
    [color black (line [((fst (find_cell_coords (x1, y1) f_size)), (snd (find_cell_coords (x1, y1) f_size)) - 10),
        ((fst (find_cell_coords (x2, y2) f_size)), (snd (find_cell_coords (x2, y2) f_size)) + 10)]),
    color black (line [((fst (find_cell_coords (x1, y1) f_size)) + 1, (snd (find_cell_coords (x1, y1) f_size)) - 10),
        ((fst (find_cell_coords (x2, y2) f_size)) + 1, (snd (find_cell_coords (x2, y2) f_size)) + 10)]),
    color black (line [((fst (find_cell_coords (x1, y1) f_size)) - 1, (snd (find_cell_coords (x1, y1) f_size)) - 10),
        ((fst (find_cell_coords (x2, y2) f_size)) - 1, (snd (find_cell_coords (x2, y2) f_size)) + 10)])])
    else if x1 < x2 then Pictures (
    [color black (line [((fst (find_cell_coords (x1, y1) f_size)) - 10, (snd (find_cell_coords (x1, y1) f_size)) - 10),
        ((fst (find_cell_coords (x2, y2) f_size)) + 10, (snd (find_cell_coords (x2, y2) f_size)) + 10)]),
    color black (line [((fst (find_cell_coords (x1, y1) f_size)) + 1 - 10,
        (snd (find_cell_coords (x1, y1) f_size)) - 10),
        ((fst (find_cell_coords (x2, y2) f_size)) + 10, (snd (find_cell_coords (x2, y2) f_size)) - 1 + 10)]),
    color black (line [((fst (find_cell_coords (x1, y1) f_size)) - 10,
        (snd (find_cell_coords (x1, y1) f_size)) + 1 - 10),
        ((fst (find_cell_coords (x2, y2) f_size)) - 1 + 10, (snd (find_cell_coords (x2, y2) f_size)) + 10)])])
    else Pictures (
    [color black (line [((fst (find_cell_coords (x1, y1) f_size)) + 10,
        (snd (find_cell_coords (x1, y1) f_size)) - 10),
        ((fst (find_cell_coords (x2, y2) f_size)) - 10, (snd (find_cell_coords (x2, y2) f_size)) + 10)]),
    color black (line [((fst (find_cell_coords (x1, y1) f_size)) + 10,
        (snd (find_cell_coords (x1, y1) f_size)) + 1 - 10),
        ((fst (find_cell_coords (x2, y2) f_size)) + 1 - 10, (snd (find_cell_coords (x2, y2) f_size)) + 10)]),
    color black (line [((fst (find_cell_coords (x1, y1) f_size)) - 1 + 10,
        (snd (find_cell_coords (x1, y1) f_size)) - 10),
        ((fst (find_cell_coords (x2, y2) f_size)) - 10, (snd (find_cell_coords (x2, y2) f_size)) - 1 + 10)])])

game_over_pic :: ScreenSize -> FieldSize -> Field -> GameState -> Picture
game_over_pic (w, h) (f_width, f_height) f g_state = Pictures (
    [game_field_pic (w, h) (f_width, f_height) f,
    translate 0 (-fromIntegral h / 2.4) (color chartreuse (polygon (rectanglePath (fromIntegral w / 3.84)
        (fromIntegral h / 8.64)))),
    translate (-fromIntegral w / 9.31) (-fromIntegral h / 2.304) (color black (scale (fromIntegral w / 6148)
        (fromIntegral h / 3456) (text "Back to main menu"))),
    winner_name_pic (w, h) g_state])

winner_name_pic :: ScreenSize -> GameState -> Picture
winner_name_pic (w, h) g_state = case g_state of
    C_ZeroMove -> translate (-fromIntegral w / 12.288) (fromIntegral h / 2.47)
        (color azure (scale (fromIntegral w / 3072) (fromIntegral h / 1728) (text "Victory!!!")))
    C_CrossMove -> translate (-fromIntegral w / 13.96) (fromIntegral h / 2.47)
        (color red (scale (fromIntegral w / 3072) (fromIntegral h / 1728) (text "Defeat!!!")))
    Draw -> translate (-fromIntegral w / 21.94) (fromIntegral h / 2.47)
        (color black (scale (fromIntegral w / 3072) (fromIntegral h / 1728) (text "Draw...")))
    ZeroMove -> translate (-fromIntegral w / 10.24) (fromIntegral h / 2.47)
        (color azure (scale (fromIntegral w / 3072) (fromIntegral h / 1728) (text "Blue wins!!!")))
    CrossMove -> translate (-fromIntegral w / 10.97) (fromIntegral h / 2.47)
        (color red (scale (fromIntegral w / 3072) (fromIntegral h / 1728) (text "Red wins!!!")))
    _ -> Blank

state_red_button_width :: ScreenSize -> Int -> Color
state_red_button_width (w, h) field_width =
    if field_width > 10  then red
    else (greyN 0.75)

state_red_button_height :: ScreenSize -> Int -> Color
state_red_button_height (w, h) field_height =
    if field_height > 10 then red
    else (greyN 0.75)

state_green_button_width :: ScreenSize -> Int -> Color
state_green_button_width (w, h) field_width =
    if field_width < (max_field_width (w, h)) then chartreuse
    else (greyN 0.75)

state_green_button_height :: ScreenSize -> Int -> Color
state_green_button_height (w, h) field_height =
    if field_height < (max_field_height (w, h)) then chartreuse
    else (greyN 0.75)
