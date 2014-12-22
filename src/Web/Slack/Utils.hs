module Web.Slack.Utils where

import           Data.Char

toSnake :: String -> String
toSnake (a:b:c)
  | isAlpha a && (isUpper b || isDigit b) = toLower a : '_' : toSnake (toLower b : c)
  | otherwise = toLower a : toSnake (b:c)
toSnake [x] = [toLower x]
toSnake [] = []


toCamel :: String -> String
toCamel ('_':x:xs) = toUpper x : toCamel xs
toCamel (x:xs) = x : toCamel xs
toCamel [] = []
