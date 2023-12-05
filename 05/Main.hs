{-# LANGUAGE OverloadedStrings #-}
import           Data.Text                      ( Text )
import           Control.Parallel.Strategies
import           Data.Function.Memoize
import           Data.Maybe                     ( mapMaybe )
import           Data.List                      ( nub )
import           Data.List.Split                ( chunksOf
                                                , splitWhen
                                                )
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

parseSeeds1 :: Text -> [Int]
parseSeeds1 line = read . T.unpack <$> T.words (T.replace "seeds: " "" line)

generateSeedRange :: [Int] -> [Int]
generateSeedRange [rangeStart, rangeLength] =
  [rangeStart .. rangeStart + rangeLength - 1]

parseSeeds2 :: Text -> [Int]
parseSeeds2 line =
  nub $ concatMap generateSeedRange $ chunksOf 2 $ parseSeeds1 line

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

