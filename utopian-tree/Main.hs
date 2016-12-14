import Control.Applicative
import Control.Monad
import System.IO

spring :: Int -> Int -> Int
spring 0 h = h
spring cycles h = summer (cycles - 1) (h * 2)

summer :: Int -> Int -> Int
summer 0 h = h
summer cycles h = spring (cycles - 1) (h + 1)

main :: IO ()
main = do
    t_temp <- getLine
    let t = read t_temp :: Int
    forM_ [1..t] $ \a0  -> do
        n_temp <- getLine
        let n = read n_temp :: Int
        putStrLn $ show $ spring n 1