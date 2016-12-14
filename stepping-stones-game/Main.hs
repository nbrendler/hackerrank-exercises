import Data.List

isSquare :: Int -> Maybe Int
isSquare n = case (truncate(sqrt(x)) * truncate(sqrt(x)) == n) of
               True -> Just $ (truncate(sqrt(x)) - 1) `div` 2
               False -> Nothing
             where x = fromIntegral n

res ::  Maybe Int -> String
res Nothing = "Better Luck Next Time"
res (Just i)  = "Go On Bob " ++ show i

process :: Int -> IO ()
process 0 = return ()
process cnt = do
    input <- getLine
    let asInt = read input :: Int
    putStrLn $ res $ isSquare $ 8 * asInt + 1
    process (cnt - 1)

main :: IO ()
main = do
    cnt <- getLine
    process (read cnt :: Int)