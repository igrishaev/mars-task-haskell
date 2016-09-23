
import qualified Data.Text as T
import Text.Printf
import Data.List


data Cmd = F | L | R deriving (Eq)
data Ori = N | E | S | W deriving (Show)
data Robot = Robot Int Int Ori deriving (Show)
data World = World Int Int [(Int, Int)]
type Route = (Int, Int, Ori, [Cmd])
type Routes = [Route]
type Result = (Bool, Robot)
type Results = [Result]


make_world :: Int -> Int -> World
make_world w h = (World w h [])


set_scent :: World -> Robot -> World
set_scent (World w h scents) (Robot x y _) = (World w h scents_new)
  where
    scents_new = scents ++ [(x, y)]


move_robot :: Robot -> Robot
move_robot (Robot x y ori) = (Robot x_new y_new ori)
  where
    (dx, dy) = case ori of
      N -> (0, 1)
      W -> (-1, 0)
      S -> (0, -1)
      E -> (1, 0)
    x_new = x + dx
    y_new = y + dy


rotate :: Ori -> Cmd -> Ori
rotate N L = W
rotate N R = E
rotate E L = N
rotate E R = S
rotate S L = E
rotate S R = W
rotate W L = S
rotate W R = N


ori_text :: Ori -> String
ori_text N = "N"
ori_text E = "E"
ori_text S = "S"
ori_text W = "W"


rotate_robot :: Robot -> Cmd -> Robot
rotate_robot (Robot x y ori) rot = (Robot x y ori_new)
  where
    ori_new = rotate ori rot


is_placed :: World -> Robot -> Bool
is_placed (World w h _) (Robot x y _) = 0 <= x && x <= w && 0 <= y && y <= h


is_scent :: World -> Robot -> Bool
is_scent (World _ _ scents) (Robot x y _) = elem (x, y) scents


update :: World -> Robot -> Cmd -> (World, Robot, Bool)
update world robot cmd
  | cmd == L || cmd == R = (world, (rotate_robot robot cmd), True)
  | cmd == F = let
      on_scent = is_scent world robot
      robot_next = move_robot robot
      placed_next = is_placed world robot_next
    in
      if on_scent
         then if placed_next
              then (world, robot_next, True)
              else (world, robot, True)
         else if placed_next
              then (world, robot_next, True)
              else ((set_scent world robot), robot, False)


robot_play :: World -> Robot -> [Cmd] -> (World, Robot, Bool)
robot_play world robot cmds = iterate world robot cmds True
  where
    iterate :: World -> Robot -> [Cmd] -> Bool -> (World, Robot, Bool)
    iterate world robot cmds flag
      | null cmds || not flag = (world, robot, flag)
      | otherwise = let
          cmd:cmds_rest = cmds
          (world_new, robot_new, flag_new) = update world robot cmd
        in
          iterate world_new robot_new cmds_rest flag_new


game_play :: Int -> Int -> [Route] -> Results
game_play w h routes = iterate world routes []
  where
    world = make_world w h
    iterate :: World -> [Route] -> Results -> Results
    iterate world routes results
      | null routes = results
      | otherwise = let
          route:routes_rest = routes
          (x, y, ori, cmds) = route
          robot = Robot x y ori
          (world_new, robot_new, flag) = robot_play world robot cmds
          results_new = results ++ [(flag, robot_new)]
        in
          iterate world_new routes_rest results_new


strip = T.unpack . T.strip . T.pack


compose_results :: Results -> String
compose_results results = intercalate "\n" nodes ++ "\n"
  where
    get_node :: Result -> String
    get_node result = strip $ printf "%d %d %s %s" x y (ori_text ori) lost
      where
        (flag, (Robot x y ori)) = result
        lost = if flag then "" else "LOST"
    nodes = map get_node results


main :: IO ()
main = putStr $ compose_results results
  where
    results = game_play 5 3 [(1, 1, E, [R, F, R, F, R, F, R, F]),
                             (3, 2, N, [F, R, R, F, L, L, F, F, R, R, F, L, L]),
                             (0, 3, W, [L, L, F, F, F, L, F, L, F, L])]
