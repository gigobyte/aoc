{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiWayIf #-}
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T
import qualified Data.Map                      as M
import qualified Data.Sequence                 as S
import           Data.Text                      ( Text )
import           Data.List                      ( nub )
import           Data.Foldable                  ( toList )
import           Data.Sequence                  ( Seq
                                                , (><)
                                                , (<|)
                                                )
import           Data.Map                       ( Map
                                                , (!)
                                                )

type Pos = (Int, Int)
type Direction = (Int, Int)
type Grid = Map Pos Char
type VisitedTile = (Pos, Direction)

up = (-1, 0)
right = (0, 1)
down = (1, 0)
left = (0, -1)

advance :: Pos -> Direction -> Pos
advance (x, y) (z, w) = (x + z, y + w)

rightMirrorMap :: Map Direction Direction
rightMirrorMap =
  M.fromList [(right, up), (left, down), (up, right), (down, left)]

leftMirrorMap :: Map Direction Direction
leftMirrorMap =
  M.fromList [(right, down), (left, up), (up, left), (down, right)]

getEnergizedTiles :: Grid -> Seq Pos
getEnergizedTiles grid = go S.empty grid (0, 0) right S.empty where
  go :: Seq VisitedTile -> Grid -> Pos -> Direction -> Seq Pos -> Seq Pos
  go visited grid position direction energized
    | (position, direction) `elem` visited = energized
    | otherwise = case M.lookup position grid of
      Just '.'  -> continue direction
      Just '/'  -> continue $ rightMirrorMap ! direction
      Just '\\' -> continue $ leftMirrorMap ! direction
      Just '|'  -> if
        | direction `elem` [up, down]    -> continue direction
        | direction `elem` [left, right] -> splitInto up down
      Just '-' -> if
        | direction `elem` [left, right] -> continue direction
        | direction `elem` [up, down]    -> splitInto left right
      Nothing -> energized
   where
    newEnergized = position <| energized
    newVisited   = (position, direction) <| visited

    continue dir = go newVisited grid (advance position dir) dir newEnergized

    splitInto dir1 dir2 =
      newEnergized
        >< go newVisited grid (advance position dir1) dir1 S.empty
        >< go newVisited grid (advance position dir2) dir2 S.empty

parseLine :: Int -> Text -> [(Pos, Char)]
parseLine i line = zipWith (\j c -> ((i, j), c)) [0 ..] (T.unpack line)

parseInput :: [Text] -> Grid
parseInput lines = M.fromList $ concat $ zipWith parseLine [0 ..] lines

solve :: Text -> Int
solve = length . nub . toList . getEnergizedTiles . parseInput . T.lines

main = do
  input <- T.readFile "./input.txt"
  print $ solve input
