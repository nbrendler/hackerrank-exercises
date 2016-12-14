import Control.Applicative
import Control.Monad
import System.IO

bubble :: (Ord a) => a -> [a] -> Int -> ([a], Bool, Int)
bubble x1 (x2:xs) cnt
  | x1 <= x2  = merge x1 False $ bubble x2 xs cnt
  | otherwise = merge x2 True $ bubble x1 xs (cnt + 1)
  where merge x s (xs, s', cnt) = (x:xs, s || s', cnt)
bubble x1 [] cnt = ([x1], False, cnt)

bubblesort :: (Ord a) => [a] -> Int -> ([a], Int)
bubblesort [] _    = ([], 0)
bubblesort (x0:xs) c = case bubble x0 xs c of
   (xs', True, cnt)  -> bubblesort xs' cnt
   (xs', False, cnt) -> (xs', cnt)

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    a_temp <- getLine
    let a = map read $ words a_temp :: [Int]
        result = bubblesort a 0
        sorted = fst result :: [Int]
        numSwaps = snd result :: Int
    putStrLn $ "Array is sorted in " ++ show numSwaps ++ " swaps."
    putStrLn $ "First Element: " ++ show (sorted !! 0)
    putStrLn $ "Last Element: " ++ show (sorted !! (n - 1))
