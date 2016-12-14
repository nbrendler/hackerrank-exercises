import Control.Applicative
import Control.Monad
import System.IO
import Data.List

anagram xs ys = length (xs \\ ys) + length (ys \\ xs)

main :: IO ()
main = do
    a <- getLine
    b <- getLine
    putStrLn $ show $ anagram a b