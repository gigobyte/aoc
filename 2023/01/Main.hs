{-# LANGUAGE OverloadedStrings #-}
import           Data.Text                      ( Text )
import           Data.Ord                       ( comparing )
import qualified Data.List                     as List
import qualified Data.Text.Internal.Search     as T
import qualified Data.Text                     as T
import qualified Data.Text.IO                  as T

digitsAsWords =
  ["one", "two", "three", "four", "five", "six", "seven", "eight", "nine"]

digitsAsText = (T.singleton <$> ['1' .. '9']) ++ digitsAsWords

textToDigit :: Text -> Int
textToDigit t =
  maybe (read (T.unpack t)) (+ 1) (List.elemIndex t digitsAsWords)

getDigitsWithIndices :: Text -> [(Text, (Int, Int))]
getDigitsWithIndices document =
  [ (digitAsText, (minimum occurancesOfDigit, maximum occurancesOfDigit))
  | digitAsText <- digitsAsText
  , let occurancesOfDigit = T.indices digitAsText document
  , not (null occurancesOfDigit)
  ]

getCalibrationValue :: Text -> Int
getCalibrationValue document =
  let allIndices       = getDigitsWithIndices document
      (firstDigit , _) = List.minimumBy (comparing (fst . snd)) allIndices
      (secondDigit, _) = List.maximumBy (comparing (snd . snd)) allIndices
  in  read $ show (textToDigit firstDigit) ++ show (textToDigit secondDigit)

solve :: Text -> Int
solve = sum . map getCalibrationValue . T.lines

main = do
  input <- T.readFile "./input.txt"
  print $ solve input
