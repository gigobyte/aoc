{-# LANGUAGE OverloadedStrings #-}
import           Data.Text                      ( Text )
import           Data.Char                      ( digitToInt
                                                , isDigit
                                                )
import           Data.List                      ( find
                                                , nub
                                                )
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

isSymbol :: Point -> Bool
isSymbol (c, _, _) = not (isDigit c) && c /= '.'

mapLine :: Int -> Text -> [Point]
mapLine idx line = mapWithIndex (\idx2 c -> (c, idx, idx2)) $ T.unpack line

mapLines :: [Text] -> [[Point]]
mapLines = mapWithIndex mapLine

addToNumber :: Int -> Char -> Int
addToNumber a b = read $ show a ++ [b]

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

isPointInsidePartNumber :: Point -> PartNumber -> Bool
isPointInsidePartNumber (_, x, y) part =
  fst (coordsStart part)
    == x
    && snd (coordsStart part)
    <= y
    && snd (coordsEnd part)
    >= y

data PartParsing
  = StartNotFound
  | InProgress PartNumber
  | Found PartNumber

getFullPartNumber :: [Point] -> Point -> PartParsing
getFullPartNumber matrix point@(_, x, _) =
  let row = mfilter (\(_, rowX, _) -> rowX == x) matrix

      findRestOfNumber :: PartParsing -> Point -> PartParsing
      findRestOfNumber acc (currC, currX, currY) = case acc of
        StartNotFound -> if isDigit currC
          then InProgress
            (PartNumber { partNumber  = digitToInt currC
                        , coordsStart = (currX, currY)
                        , coordsEnd   = (currX, currY)
                        }
            )
          else StartNotFound
        InProgress part -> if isDigit currC
          then InProgress
            (PartNumber { partNumber  = addToNumber (partNumber part) currC
                        , coordsStart = coordsStart part
                        , coordsEnd   = (currX, currY)
                        }
            )
          else if isPointInsidePartNumber point part
            then Found part
            else StartNotFound
        Found _ -> acc
  in  foldl findRestOfNumber StartNotFound row

getCharAtPos :: [Point] -> (Int, Int) -> Maybe Point
getCharAtPos matrix (x, y) = find (\(_, x2, y2) -> x == x2 && y == y2) matrix

-- getPartNumbers' :: [Point] -> [PartNumber] -> Point -> [PartNumber]
-- getPartNumbers' matrix acc point@(c, x, y)
--   | isDigit c && hasSymbolNeighbour matrix point
--   = acc ++ [ part | (Found part) <- [getFullPartNumber matrix point] ]
--   | otherwise
--   = acc

getPartNumbers :: Row -> [PartNumber]
getPartNumbers =
  let go :: [PartNumber] -> Point -> [PartNumber]
      go = undefined
  in  foldl go []

-- solve :: Text -> Int
solve = map getPartNumbers . mapLines . T.lines

main = do
  input <- T.readFile "./input.txt"
  print $ solve input
