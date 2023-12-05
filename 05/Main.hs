{-# LANGUAGE OverloadedStrings #-}
import           Data.Text                      ( Text )
import           Data.Maybe                     ( mapMaybe )
import           Data.List                      ( nub )
import           Control.Parallel.Strategies
import           Data.Function.Memoize
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

group :: Int -> [a] -> [[a]]
group _ []        = []
group n l | n > 0 = take n l : group n (drop n l)

splitWhen :: (a -> Bool) -> [a] -> [[a]]
splitWhen p xs = go xs []
 where
  go []       acc = [acc]
  go (y : ys) acc = if p y then acc : go ys [] else go ys (acc ++ [y])

parseSeeds1 :: Text -> [Int]
parseSeeds1 line = read . T.unpack <$> T.words (T.replace "seeds: " "" line)

generateSeedRange :: [Int] -> [Int]
generateSeedRange [rangeStart, rangeLength] =
  [rangeStart .. rangeStart + rangeLength - 1]

parseSeeds2 :: Text -> [Int]
parseSeeds2 line =
  nub $ concatMap generateSeedRange $ group 2 $ parseSeeds1 line

parseMap :: Text -> [Int]
parseMap = map (read . T.unpack) . T.words

parseMaps :: [Text] -> [[[Int]]]
parseMaps lines = map parseMap . drop 1 <$> splitWhen (== "") lines

parseInput :: (Text -> [Int]) -> [Text] -> ([Int], [[[Int]]])
parseInput seedParser lines =
  (seedParser $ head lines, parseMaps (drop 2 lines))

findSourceInMap :: Int -> [Int] -> Maybe Int
findSourceInMap source [destRangeStart, sourceRangeStart, rangeLength]
  | source >= sourceRangeStart && source < sourceRangeStart + rangeLength
  = Just $ source - sourceRangeStart + destRangeStart
  | otherwise
  = Nothing

getSourceDestination' :: Int -> [[Int]] -> Int
getSourceDestination' source maps =
  case mapMaybe (findSourceInMap source) maps of
    []     -> source
    [dest] -> dest

getSourceDestination = memoize2 getSourceDestination'

findLocationNumber :: [[[Int]]] -> Int -> Int
findLocationNumber maps seed = foldl getSourceDestination seed maps

findLocationNumbers :: ([Int], [[[Int]]]) -> [Int]
findLocationNumbers (seeds, maps) =
  map (findLocationNumber maps) seeds `using` parListChunk 64 rdeepseq

solve1 :: Text -> Int
solve1 = minimum . findLocationNumbers . parseInput parseSeeds1 . T.lines

solve2 :: Text -> Int
solve2 = minimum . findLocationNumbers . parseInput parseSeeds2 . T.lines

main = do
  input <- T.readFile "./input.txt"
  print $ solve2 input

