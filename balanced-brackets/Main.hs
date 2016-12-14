import Control.Applicative
import Control.Monad
import System.IO
import Data.List

process :: [Char] -> Char -> [Char]
process ('{':xs) '}' = xs
process ('[':xs) ']' = xs
process ('(':xs) ')' = xs
process xs c = c:xs

isBalanced :: String -> String
isBalanced s = case (foldl' process [] s) of
                 [] -> "YES"
                 _  -> "NO"

main :: IO ()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    forM_ [1..t] $ \a0  -> do
        expression <- getLine
        putStrLn $ isBalanced expression
