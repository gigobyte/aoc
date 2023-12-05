{-# LANGUAGE OverloadedStrings #-}
import           Data.Text                      ( Text )
import           Data.Maybe                     ( mapMaybe )
import           Debug.Trace
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = go xs []
 where
  go []       acc = [acc]
  go (y : ys) acc = if p y then acc : go ys [] else go ys (acc ++ [y])

parseSeeds :: Text -> [Int]
parseSeeds line = read . T.unpack <$> T.words (T.replace "seeds: " "" line)

parseMap :: Text -> [Int]
parseMap = map (read . T.unpack) . T.words

parseMaps :: [Text] -> [[[Int]]]
parseMaps lines = map parseMap . drop 1 <$> splitWhen (== "") lines

parseInput :: [Text] -> ([Int], [[[Int]]])
parseInput lines = (parseSeeds $ head lines, parseMaps (drop 2 lines))

findSourceInMap :: Int -> [Int] -> Maybe Int
findSourceInMap source [destRangeStart, sourceRangeStart, rangeLength]
  | source >= sourceRangeStart && source < sourceRangeStart + rangeLength
  = Just $ (source - sourceRangeStart) + destRangeStart
  | otherwise
  = Nothing

getSourceDestination :: Int -> [[Int]] -> Int
getSourceDestination source maps =
  case mapMaybe (findSourceInMap source) maps of
    []     -> source
    [dest] -> dest

findLocationNumber :: [[[Int]]] -> Int -> Int
findLocationNumber maps seed = foldl getSourceDestination seed maps

findLocationNumbers :: ([Int], [[[Int]]]) -> [Int]
findLocationNumbers (seeds, maps) = findLocationNumber maps <$> seeds

solve = minimum . findLocationNumbers . parseInput . T.lines

main = do
  input <- T.readFile "./input.txt"
  print $ solve input

