module Monitoring (
    check_draw,
    check_win
    ) where

import Graphics.Gloss.Interface.Environment
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Interact
import Data.Map
import Data.List
import System.Random
import Types

check_draw :: Field -> FieldSize -> Bool
check_draw f (field_width, field_height) = check_draw1 (1, 1) f where
    check_draw1 (w, h) g_f = if Data.Map.lookup (w, h) g_f == Just Empty then False
                             else if h == field_height && w == field_width then True
                             else if w == field_width then check_draw1 (1, h + 1) g_f
                             else check_draw1 (w + 1, h) g_f

check_win :: Field -> (Int, Int) -> FieldSize -> ((Int, Int), (Int, Int))
check_win f (x, y) (field_width, field_height) = if check_horizon then ((x, y), (x + 4, y))
                     else if check_vertical then ((x, y), (x, y + 4))
                     else if check_diagonal_r then ((x, y), (x + 4, y + 4))
                     else if check_diagonal_l then ((x, y), (x - 4, y + 4))
                     else if y == field_height && x == field_width then ((0, 0), (0, 0))
                     else if x == field_width then (check_win f (1, y + 1) (field_width, field_height))
                     else (check_win f (x + 1, y) (field_width, field_height)) where
                        check_horizon = if x + 4 > field_width then False
                                        else Data.Map.lookup (x, y) f == Data.Map.lookup (x + 1, y) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x + 2, y) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x + 3, y) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x + 4, y) f
                                        && Data.Map.lookup (x, y) f /= Just Empty
                        check_vertical = if y + 4 > field_height then False
                                        else Data.Map.lookup (x, y) f == Data.Map.lookup (x, y + 1) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x, y + 2) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x, y + 3) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x, y + 4) f
                                        && Data.Map.lookup (x, y) f /= Just Empty
                        check_diagonal_r = if y + 4 > field_height && x + 4 > field_width then False
                                        else Data.Map.lookup (x, y) f == Data.Map.lookup (x + 1, y + 1) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x + 2, y + 2) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x + 3, y + 3) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x + 4, y + 4) f
                                        && Data.Map.lookup (x, y) f /= Just Empty
                        check_diagonal_l = if y + 4 > field_height && x - 4 < 1 then False
                                        else Data.Map.lookup (x, y) f == Data.Map.lookup (x - 1, y + 1) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x - 2, y + 2) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x - 3, y + 3) f
                                        && Data.Map.lookup (x, y) f == Data.Map.lookup (x - 4, y + 4) f
                                        && Data.Map.lookup (x, y) f /= Just Empty
