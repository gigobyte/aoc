{-# LANGUAGE OverloadedStrings #-}
import           Data.Text                      ( Text )
import           Data.Attoparsec.Text
import           Data.Either                    ( rights )
import           Data.Maybe                     ( mapMaybe )
import qualified Data.List.Safe                as List
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

data Cube = Red Int | Green Int | Blue Int deriving (Show)

type CubeSet = [Cube]
type Game = (Int, [CubeSet])

-- Parser
parseCube :: Parser Cube
parseCube = do
  skipSpace
  choice
    [ Red <$> (decimal <* space <* string "red")
    , Green <$> (decimal <* space <* string "green")
    , Blue <$> (decimal <* space <* string "blue")
    ]

parseCubeSet :: Parser CubeSet
parseCubeSet = parseCube `sepBy` char ','

parseGame :: Parser Game
parseGame = do
  string "Game "
  gameId <- decimal
  char ':'
  cubeSets <- parseCubeSet `sepBy` char ';'
  return (gameId, cubeSets)

parseInput :: Parser [Game]
parseInput = parseGame `sepBy` endOfLine

unsafeParseInput :: Text -> [Game]
unsafeParseInput = either (const undefined) id . parseOnly parseInput
-- Parser

isValidCubeCount :: Cube -> Bool
isValidCubeCount (Red   n) = n <= 12
isValidCubeCount (Green n) = n <= 13
isValidCubeCount (Blue  n) = n <= 14

isValidGame :: Game -> Bool
isValidGame (_, sets) = all (all isValidCubeCount) sets

getRed :: CubeSet -> Maybe Int
getRed set = List.head [ x | (Red x) <- set ]

getGreen :: CubeSet -> Maybe Int
getGreen set = List.head [ x | (Green x) <- set ]

getBlue :: CubeSet -> Maybe Int
getBlue set = List.head [ x | (Blue x) <- set ]

getGamePower :: Game -> Int
getGamePower (_, sets) = product
  [ maximum (mapMaybe getRed sets)
  , maximum (mapMaybe getGreen sets)
  , maximum (mapMaybe getBlue sets)
  ]

solve1 :: Text -> Int
solve1 = sum . map fst . filter isValidGame . unsafeParseInput

solve2 :: Text -> Int
solve2 = sum . map getGamePower . unsafeParseInput

main = do
  input <- T.readFile "./input.txt"
  print $ solve1 input
  print $ solve2 input
