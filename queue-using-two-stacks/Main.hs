-- Enter your code here. Read input from STDIN. Print output to STDOUT
import Control.Monad

data Queue a = Queue { inbox :: [a], outbox :: [a] } deriving (Show)

enqueue :: a -> Queue a -> Queue a
enqueue n (Queue i out) = Queue (n:i) out

checkQueue :: Queue a -> Queue a
checkQueue (Queue i [])  = Queue [] (reverse i)
checkQueue q = q

peek :: Queue a -> a
peek (Queue _ out) = head out

dequeue :: Queue a -> Queue a
dequeue (Queue i out) = Queue i (tail out)

process :: String -> [String] -> Queue Int -> IO (Queue Int)
process command xs q'
    | command == "1" = return $ enqueue (read $ head xs :: Int) q
    | command == "2" = return $ dequeue q
    | command == "3" = do
        putStrLn $ show $ peek q
        return q
    where q = checkQueue q'
        
loop :: Queue Int -> Int -> IO (Queue Int)
loop q _ = do
    line <- getLine
    let query = words line
    let command = head query
    process command (tail query) q

main :: IO ()
main = do
    t_temp <- getLine
    let numQueries = read t_temp :: Int
    foldM_ loop (Queue [] [] :: Queue Int) [1..numQueries] 