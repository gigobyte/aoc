{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
import           Data.Text                      ( Text )
import           Data.Char                      ( digitToInt
                                                , isDigit
                                                )
import           Data.List                      ( find )
import           Control.Monad                  ( mfilter )
import           Data.Maybe                     ( isJust )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

type Point = (Char, Int, Int)
type Row = [Point]
type Gear = (Int, Int)

data PartNumber = PartNumber
  { partNumber :: Int
  , coordsStart :: (Int, Int)
  , coordsEnd :: (Int, Int)
  } deriving (Eq, Show)

data SchematicInfo = Part PartNumber | Gear Gear deriving (Eq, Show)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex = flip zipWith [0 ..]

getPartEndCoordsY :: PartNumber -> Int
getPartEndCoordsY PartNumber { coordsEnd } = snd coordsEnd

getPartAllPoints :: PartNumber -> [Point]
getPartAllPoints PartNumber { coordsStart, coordsEnd } =
  ('_', fst coordsStart, ) <$> [snd coordsStart .. snd coordsEnd]

addToNumber :: Int -> Int -> Int
addToNumber a b = read $ show a ++ show b

mergeParts :: PartNumber -> PartNumber -> PartNumber
mergeParts part1 part2 = PartNumber
  (addToNumber (partNumber part1) (partNumber part2))
  (coordsStart part1)
  (coordsEnd part2)

upsertPointCoordinates :: [SchematicInfo] -> PartNumber -> [SchematicInfo]
upsertPointCoordinates []    part = [Part part]
upsertPointCoordinates infos part = case last infos of
  Part p -> if getPartEndCoordsY p == getPartEndCoordsY part - 1
    then init infos ++ [Part $ mergeParts p part]
    else infos ++ [Part part]
  Gear _ -> infos ++ [Part part]

mapRow :: Int -> Text -> Row
mapRow idx line = mapWithIndex (\idx2 c -> (c, idx, idx2)) $ T.unpack line

mapRows :: [Text] -> [Row]
mapRows = mapWithIndex mapRow

getCharAtPos :: [Point] -> (Int, Int) -> Maybe Point
getCharAtPos matrix (x, y) = find (\(_, x2, y2) -> x == x2 && y == y2) matrix

isSymbol :: Point -> Bool
isSymbol (c, _, _) = not (isDigit c) && c /= '.'

hasAdjacent :: (Point -> Bool) -> [Point] -> Point -> Bool
hasAdjacent cond matrix (c, x, y) = any
  (\coords -> isJust (mfilter cond $ getCharAtPos matrix coords))
  [ (x + 1, y)
  , (x - 1, y)
  , (x    , y - 1)
  , (x    , y + 1)
  , (x + 1, y + 1)
  , (x + 1, y - 1)
  , (x - 1, y + 1)
  , (x - 1, y - 1)
  ]

hasAdjacentSymbol :: [Point] -> PartNumber -> Bool
hasAdjacentSymbol matrix part =
  any (hasAdjacent isSymbol matrix) (getPartAllPoints part)

isAdjacentPartNumber :: Gear -> PartNumber -> Bool
isAdjacentPartNumber (gearX, gearY) part =
  hasAdjacent (const True) (getPartAllPoints part) ('_', gearX, gearY)

getSchematicInfo :: Row -> [SchematicInfo]
getSchematicInfo =
  let go :: [SchematicInfo] -> Point -> [SchematicInfo]
      go acc (currC, currX, currY)
        | isDigit currC
        = let newPartCoords = (currX, currY)
              newPart =
                  PartNumber (digitToInt currC) newPartCoords newPartCoords
          in  upsertPointCoordinates acc newPart
        | currC == '*'
        = acc ++ [Gear (currX, currY)]
        | otherwise
        = acc
  in  foldl go []

getValidPartNumbers :: [Row] -> [PartNumber]
getValidPartNumbers rows =
  [ x
  | (Part x) <- concatMap getSchematicInfo rows
  , hasAdjacentSymbol (concat rows) x
  ]

getGearRatio :: [PartNumber] -> Gear -> Int
getGearRatio parts gear = case mfilter (isAdjacentPartNumber gear) parts of
  [part1, part2] -> partNumber part1 * partNumber part2
  _              -> 0

solve1 :: Text -> Int
solve1 input =
  let rows             = mapRows $ T.lines input
      validPartNumbers = getValidPartNumbers rows
  in  sum $ partNumber <$> validPartNumbers

solve2 :: Text -> Int
solve2 input =
  let rows        = mapRows $ T.lines input
      matrix      = concat rows
      info        = concatMap getSchematicInfo rows
      partNumbers = [ x | (Part x) <- info ]
      gears       = [ x | (Gear x) <- info ]
  in  sum $ getGearRatio partNumbers <$> gears

main = do
  input <- T.readFile "./input.txt"
  print $ solve1 input
  print $ solve2 input
