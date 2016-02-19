module CodeOff1 where

import           Control.Monad   (replicateM)
import           Data.Char       (ord)
import           Data.Foldable   (for_, traverse_)
import           Data.List       (delete)
import           Data.Map.Strict (Map, fromListWith, (!))

charValue :: Char -> Int
charValue c
    | 'A' <= c, c <= 'Z' = ord c - ord 'A' + 1
    | 'a' <= c, c <= 'z' = ord c - ord 'a'
    | otherwise = error ("charValue: unexpected character: " ++ show c)

value :: String -> Int
value = sum . map charValue

byValue :: [String] -> Map Int [String]
byValue lines = fromListWith (++) [(value s, [s]) | s <- lines]

main :: IO ()
main = do
    n <- readLn
    lines <- replicateM n getLine

    let valueMap = byValue lines
    let equalStrings s = delete s (valueMap ! value s)

    for_ lines $ \s -> do
        putStrLn s
        putStrLn (if s == reverse s then "true" else "false")
        traverse_ putStrLn (equalStrings s)
