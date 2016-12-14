import Control.Applicative
import Control.Monad
import System.IO

walk :: [Int] -> Int -> Maybe Int
walk [] loaves = Just loaves
walk (x:[]) loaves = case (even x) of
    True -> Just loaves
    False -> Nothing
walk (x:y:xs) loaves = case (even x) of
  True -> walk (y:xs) loaves
  False -> walk ((y + 1):xs) (loaves + 2)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    b_temp <- getLine
    let b = map read $ words b_temp :: [Int]
    case walk b 0 of
        Just x -> putStrLn $ show x
        Nothing -> putStrLn "NO"