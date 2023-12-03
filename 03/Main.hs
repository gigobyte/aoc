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

data PartNumber = PartNumber
  { partNumber :: Int
  , coordsStart :: (Int, Int)
  , coordsEnd :: (Int, Int)
  } deriving (Eq, Show)

mapWithIndex :: (Int -> a -> b) -> [a] -> [b]
mapWithIndex = flip zipWith [0 ..]

getPartEndCoordsY :: PartNumber -> Int
getPartEndCoordsY PartNumber { coordsEnd } = snd coordsEnd

getPartAllPoints :: PartNumber -> [Point]
getPartAllPoints PartNumber { coordsStart, coordsEnd } =
  ('_', fst coordsStart, ) <$> [snd coordsStart .. snd coordsEnd]

upsertPointCoordinates :: [PartNumber] -> PartNumber -> [PartNumber]
upsertPointCoordinates [] part = [part]
upsertPointCoordinates parts part =
  let lastPart = last parts
  in  if getPartEndCoordsY lastPart == getPartEndCoordsY part - 1
        then
          take (length parts - 1) parts
            ++ [ PartNumber
                   { partNumber  = addToNumber (partNumber lastPart)
                                               (partNumber part)
                   , coordsStart = coordsStart lastPart
                   , coordsEnd   = coordsEnd part
                   }
               ]
        else parts ++ [part]

isSymbol :: Point -> Bool
isSymbol (c, _, _) = not (isDigit c) && c /= '.'

mapRow :: Int -> Text -> Row
mapRow idx line = mapWithIndex (\idx2 c -> (c, idx, idx2)) $ T.unpack line

mapRows :: [Text] -> [Row]
mapRows = mapWithIndex mapRow

addToNumber :: Int -> Int -> Int
addToNumber a b = read $ show a ++ show b

getCharAtPos :: [Point] -> (Int, Int) -> Maybe Point
getCharAtPos matrix (x, y) = find (\(_, x2, y2) -> x == x2 && y == y2) matrix

hasSymbolNeighbour :: [Point] -> Point -> Bool
hasSymbolNeighbour matrix (c, x, y) = any
  (\coords -> isJust (mfilter isSymbol $ getCharAtPos matrix coords))
  [ (x + 1, y)
  , (x - 1, y)
  , (x    , y - 1)
  , (x    , y + 1)
  , (x + 1, y + 1)
  , (x + 1, y - 1)
  , (x - 1, y + 1)
  , (x - 1, y - 1)
  ]

isPartNumberWithNeighbour :: [Point] -> PartNumber -> Bool
isPartNumberWithNeighbour matrix part =
  any (hasSymbolNeighbour matrix) (getPartAllPoints part)

getPartNumbers :: Row -> [PartNumber]
getPartNumbers =
  let go :: [PartNumber] -> Point -> [PartNumber]
      go acc (currC, currX, currY) = if isDigit currC
        then
          let newPart =
                PartNumber (digitToInt currC) (currX, currY) (currX, currY)
          in  upsertPointCoordinates acc newPart
        else acc
  in  foldl go []

getValidPartNumbers :: [Point] -> [PartNumber] -> [PartNumber]
getValidPartNumbers matrix = mfilter (isPartNumberWithNeighbour matrix)

solve :: Text -> Int
solve input =
  let rows           = mapRows $ T.lines input
      matrix         = concat rows
      allPartNumbers = concatMap getPartNumbers rows
  in  sum $ partNumber <$> getValidPartNumbers matrix allPartNumbers

main = do
  input <- T.readFile "./input.txt"
  print $ solve input
