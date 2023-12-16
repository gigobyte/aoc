{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
import           Data.Text                      ( Text )
import           Data.Map                       ( Map
                                                , (!)
                                                )
import           Data.List                      ( nub )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Map                      as M
import           Debug.Trace

data Direction = Up | Down | LLeft | RRight deriving (Show, Eq, Ord)

type Pos = (Int, Int)
type Grid = Map Pos Char
type VisitedTile = (Pos, Direction)

advance :: Pos -> Direction -> Pos
advance (x, y) Up     = (x - 1, y)
advance (x, y) RRight = (x, y + 1)
advance (x, y) Down   = (x + 1, y)
advance (x, y) LLeft  = (x, y - 1)

rightMirrorMap :: Map Direction Direction
rightMirrorMap =
  M.fromList [(RRight, Up), (LLeft, Down), (Up, RRight), (Down, LLeft)]

leftMirrorMap :: Map Direction Direction
leftMirrorMap =
  M.fromList [(RRight, Down), (LLeft, Up), (Up, LLeft), (Down, RRight)]

getEnergizedTiles :: Grid -> [Pos]
getEnergizedTiles grid = go [] grid (0, 0) RRight [] where
  go :: [VisitedTile] -> Grid -> Pos -> Direction -> [Pos] -> [Pos]
  go visited grid position direction energized
    | (position, direction) `elem` visited = energized
    | otherwise = case M.lookup position grid of
      Just '.'  -> continue direction
      Just '/'  -> continue $ rightMirrorMap ! direction
      Just '\\' -> continue $ leftMirrorMap ! direction
      Just '|'  -> if
        | direction `elem` [Up, Down]      -> continue direction
        | direction `elem` [LLeft, RRight] -> splitInto Up Down
      Just '-' -> if
        | direction `elem` [LLeft, RRight] -> continue direction
        | direction `elem` [Up, Down]      -> splitInto LLeft RRight
      Nothing -> energized
   where
    newEnergized = position : energized
    newVisited   = (position, direction) : visited

    continue dir = go newVisited grid (advance position dir) dir newEnergized

    splitInto dir1 dir2 =
      newEnergized
        ++ go newVisited grid (advance position dir1) dir1 []
        ++ go newVisited grid (advance position dir2) dir2 []

parseLine :: Int -> Text -> [(Pos, Char)]
parseLine i line = zipWith (\j c -> ((i, j), c)) [0 ..] (T.unpack line)

parseInput :: [Text] -> Grid
parseInput lines = M.fromList $ concat $ zipWith parseLine [0 ..] lines

solve :: Text -> Int
solve = length . nub . getEnergizedTiles . parseInput . T.lines

main = do
  input <- T.readFile "./input.txt"
  print $ solve input
