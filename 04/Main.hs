import           Data.Bifunctor                 ( second )
import           Data.List                      ( intersect )

parseCard :: String -> ([String], [String])
parseCard = span (/= "|") . drop 2 . words

countMatches :: ([String], [String]) -> Int
countMatches = length . uncurry intersect

calculatePoints :: Int -> Int
calculatePoints 0       = 0
calculatePoints matches = 2 ^ (matches - 1)

getScratchcards :: [([String], [String])] -> [Int]
getScratchcards = foldr (\c l -> sum (take (countMatches c) l) + 1 : l) []

solve1 :: String -> Int
solve1 = sum . fmap (calculatePoints . countMatches . parseCard) . lines

solve2 :: String -> Int
solve2 = sum . getScratchcards . fmap parseCard . lines

main = do
  input <- readFile "./input.txt"
  print $ solve1 input
  print $ solve2 input
