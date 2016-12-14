import Control.Applicative
import Control.Monad
import System.IO

checkN :: Int -> Int -> Int -> Int
checkN _ 0 res = res
checkN n t res =
    case (t `rem` 10) of
        0 -> checkN n (t `div` 10) res
        d -> case (n `rem` d) of
            0 -> checkN n (t `div` 10) (res + 1)
            _ -> checkN n (t `div` 10) res

main :: IO ()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    forM_ [1..t] $ \a0  -> do
        n_temp <- getLine
        let n = read n_temp :: Int
        putStrLn $ show $ checkN n n 0