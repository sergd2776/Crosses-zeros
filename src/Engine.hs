module Engine (
    updater
    ) where

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Map
import Data.List
import System.Random
import Types
import Monitoring

updater :: Float -> Game -> Game
updater _ (Game state g_state g_field scr_size r_g f_size) = case state of
    HumanMove -> if check_draw g_field f_size then (Game GameOver Draw g_field scr_size r_g f_size)
                 else if (fst (fst (check_win g_field (1, 1) f_size))) /= 0 then
                    (Game GameOver g_state g_field scr_size r_g f_size)
                 else (Game state g_state g_field scr_size r_g f_size)
    ComputerMove -> if check_draw g_field f_size then (Game GameOver Draw g_field scr_size r_g f_size)
                    else if (fst (fst (check_win g_field (1, 1) f_size))) /= 0 then
                        (Game GameOver g_state g_field scr_size r_g f_size)
                    else main_engine (Game state g_state g_field scr_size r_g f_size)
    _ -> (Game state g_state g_field scr_size r_g f_size)

main_engine :: Game -> Game
main_engine (Game state g_state g_field scr_size r_g f_size) =
        (comp_win (Game HumanMove C_CrossMove
        (Data.Map.insert (count_best_move g_field (rand_list r_g f_size) f_size) Zero g_field)
        scr_size (gen_skip r_g f_size) f_size))

-- count_best_move :: Field -> StdGen -> (Int, Int)
-- count_best_move g_field r_g =
--        (count_best_ratio (Data.Map.fromList [((x, y), (count_one_ratio (x, y) g_field 5)) | x <- [1 .. field_width],
--        y <- [1 .. field_height]]))

count_best_move :: Field -> [Int] -> FieldSize -> (Int, Int)
count_best_move g_field r_l (field_width, field_height) = count_best_move1 (1, 1)
    (Data.Map.empty :: Map (Int, Int) Int) g_field r_l where
        count_best_move1 (w, h) m f (i:l) =
            if h == field_height && w == field_width then
            (count_best_ratio (Data.Map.insert (w, h) (count_one_ratio (w, h) f i) m) (field_width, field_height))
            else if w == field_width then
            count_best_move1 (1, h + 1) (Data.Map.insert (w, h) (count_one_ratio (w, h) f i) m) f l
            else count_best_move1 (w + 1, h) (Data.Map.insert (w, h) (count_one_ratio (w, h) f i) m) f l

rand_list :: StdGen -> FieldSize -> [Int]
rand_list r_g (field_width, field_height) = rand_list1 1 r_g where
    rand_list1 i r =
        if i < field_width * field_height then
        (fst (System.Random.randomR (1 :: Int, 15 :: Int) r) :: Int):(rand_list1 (i + 1)
        (snd (System.Random.randomR (1 :: Int, 15 :: Int) r) :: StdGen))
        else [(fst (System.Random.randomR (1 :: Int, 15 :: Int) r) :: Int)]

gen_skip :: StdGen -> FieldSize -> StdGen
gen_skip r_g (field_width, field_height) = gen_skip1 1 r_g where
    gen_skip1 i r = if i < field_width * field_height then gen_skip1 (i + 1)
                    (snd (System.Random.randomR (1 :: Int, 15 :: Int) r) :: StdGen)
                    else (snd (System.Random.randomR (1 :: Int, 15 :: Int) r) :: StdGen)

count_one_ratio :: (Int, Int) -> Field -> Int -> Int
count_one_ratio (w, h) g_field r =
    if Data.Map.lookup (w, h) g_field /= Just Empty then -1
    else pattern_sum ([convert_cell_to_char (Data.Map.lookup (x, h) g_field) | x <- [w - 4 .. w - 1]] ++ ['0']
            ++ [convert_cell_to_char (Data.Map.lookup (x, h) g_field) | x <- [w + 1 .. w + 4]])
       + pattern_sum ([convert_cell_to_char (Data.Map.lookup (w, x) g_field) | x <- [h - 4 .. h - 1]] ++ ['0']
            ++ [convert_cell_to_char (Data.Map.lookup (w, x) g_field) | x <- [h + 1 .. h + 4]])
       + pattern_sum ([convert_cell_to_char (Data.Map.lookup (w + x, h + x) g_field) | x <- [- 4 .. - 1]] ++ ['0']
            ++ [convert_cell_to_char (Data.Map.lookup (w + x, h + x) g_field) | x <- [1 .. 4]])
       + pattern_sum ([convert_cell_to_char (Data.Map.lookup (w - x, h + x) g_field) | x <- [- 4 .. - 1]] ++ ['0']
            ++ [convert_cell_to_char (Data.Map.lookup (w - x, h + x) g_field) | x <- [1 .. 4]])
       + pattern_sum ([convert_cell_to_char (Data.Map.lookup (x, h) g_field) | x <- [w - 4 .. w - 1]] ++ ['x']
            ++ [convert_cell_to_char (Data.Map.lookup (x, h) g_field) | x <- [w + 1 .. w + 4]])
       + pattern_sum ([convert_cell_to_char (Data.Map.lookup (w, x) g_field) | x <- [h - 4 .. h - 1]] ++ ['x']
            ++ [convert_cell_to_char (Data.Map.lookup (w, x) g_field) | x <- [h + 1 .. h + 4]])
       + pattern_sum ([convert_cell_to_char (Data.Map.lookup (w + x, h + x) g_field) | x <- [- 4 .. - 1]] ++ ['x']
            ++ [convert_cell_to_char (Data.Map.lookup (w + x, h + x) g_field) | x <- [1 .. 4]])
       + pattern_sum ([convert_cell_to_char (Data.Map.lookup (w - x, h + x) g_field) | x <- [- 4 .. - 1]] ++ ['x']
            ++ [convert_cell_to_char (Data.Map.lookup (w - x, h + x) g_field) | x <- [1 .. 4]]) + r where
                    convert_cell_to_char a = case a of
                        Nothing -> '#'
                        Just Cross -> 'x'
                        Just Zero -> '0'
                        Just Empty -> '_'
                    pattern_sum l = if Data.List.isInfixOf "00000" l then 100000
                              else if Data.List.isInfixOf "xxxxx" l then 90000
                              else if Data.List.isInfixOf "_0000_" l then 10000
                              else if Data.List.isInfixOf "_xxxx_" l then 9000
                              else if Data.List.isInfixOf "0000_" l then 5000
                              else if Data.List.isInfixOf "_0000" l then 5000
                              else if Data.List.isInfixOf "xxxx_" l then 4500
                              else if Data.List.isInfixOf "_xxxx" l then 4500
                              else if Data.List.isInfixOf "0_000" l then 4000
                              else if Data.List.isInfixOf "00_00" l then 4000
                              else if Data.List.isInfixOf "000_0" l then 4000
                              else if Data.List.isInfixOf "x_xxx" l then 3600
                              else if Data.List.isInfixOf "xx_xx" l then 3600
                              else if Data.List.isInfixOf "xxx_x" l then 3600
                              else if Data.List.isInfixOf "__000___" l then 1000
                              else if Data.List.isInfixOf "___000__" l then 1000
                              else if Data.List.isInfixOf "__xxx___" l then 900
                              else if Data.List.isInfixOf "___xxx__" l then 900
                              else if Data.List.isInfixOf "__000__" l then 800
                              else if Data.List.isInfixOf "__xxx__" l then 720
                              else if Data.List.isInfixOf "_000__" l then 750
                              else if Data.List.isInfixOf "__000_" l then 750
                              else if Data.List.isInfixOf "_xxx__" l then 675
                              else if Data.List.isInfixOf "__xxx_" l then 675
                              else if Data.List.isInfixOf "000__" l then 500
                              else if Data.List.isInfixOf "_000_" l then 500
                              else if Data.List.isInfixOf "__000" l then 500
                              else if Data.List.isInfixOf "xxx__" l then 450
                              else if Data.List.isInfixOf "_xxx_" l then 450
                              else if Data.List.isInfixOf "__xxx" l then 450
                              else if Data.List.isInfixOf "0_00_" l then 250
                              else if Data.List.isInfixOf "_00_0" l then 250
                              else if Data.List.isInfixOf "00_0_" l then 250
                              else if Data.List.isInfixOf "_0_00" l then 250
                              else if Data.List.isInfixOf "0__00" l then 250
                              else if Data.List.isInfixOf "00__0" l then 250
                              else if Data.List.isInfixOf "x_xx_" l then 225
                              else if Data.List.isInfixOf "_xx_x" l then 225
                              else if Data.List.isInfixOf "xx_x_" l then 225
                              else if Data.List.isInfixOf "_x_xx" l then 225
                              else if Data.List.isInfixOf "x__xx" l then 225
                              else if Data.List.isInfixOf "xx__x" l then 225
                              else if Data.List.isInfixOf "___00___" l then 100
                              else if Data.List.isInfixOf "___xx___" l then 90
                              else if Data.List.isInfixOf "_00__" l then 70
                              else if Data.List.isInfixOf "__00_" l then 70
                              else if Data.List.isInfixOf "_xx__" l then 63
                              else if Data.List.isInfixOf "__xx_" l then 63
                              else if Data.List.isInfixOf "____0____" l then 20
                              else 0

count_best_ratio :: FieldRatio -> FieldSize -> (Int, Int)
count_best_ratio f_ratio (field_width, field_height) = count_best_ratio1 f_ratio (1, 1) (2, 1) where
        count_best_ratio1 r (w, h) (x, y) = if x == 0 && y == 0 then (w, h)
                                            else if Data.Map.lookup (x, y) r > Data.Map.lookup (w, h) r then
                                            count_best_ratio1 r (x, y) (next_key (x, y))
                                            else count_best_ratio1 r (w, h) (next_key (x, y)) where
                                                next_key (m, n) = if n == field_height && m == field_width then (0, 0)
                                                                  else if m == field_width then (1, n + 1)
                                                                  else (m + 1, n)

comp_win :: Game -> Game
comp_win (Game state g_state g_field scr_size r_g f_size) =
        if check_draw g_field f_size then (Game GameOver Draw g_field scr_size r_g f_size)
        else if (fst (fst (check_win g_field (1, 1) f_size))) /= 0 then
        (Game GameOver g_state g_field scr_size r_g f_size)
        else (Game state g_state g_field scr_size r_g f_size)

