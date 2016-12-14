import Control.Applicative
import Control.Monad
import System.IO
import Data.List

data Trie a = EmptyTrie | Root [Trie a] | Node a [Trie a] deriving (Eq, Show)

type Counter = Trie (Char, Int)

trieRoot :: Counter
trieRoot = Root []

partitionTrieList :: Char -> [Counter] -> ([Counter], [Counter])
partitionTrieList val ts = case f of
  ([], ts) -> ([Node (val, 1) []], ts)
  ([Node (v, c) innerts], ts) -> ([Node (v, c+1) innerts], ts)
  where f = partition (\(Node v _) -> (fst v) == val) ts

ins :: String -> Counter -> Counter
ins [] t = t
ins [v] EmptyTrie = Node (v, 1) []
ins (v:vs) EmptyTrie = Node (v, 1) [ins vs EmptyTrie]
ins (v:vs) (Root ts) = Root ((ins vs node):others)
  where (nodes, others) = partitionTrieList v ts
        node = head nodes
ins (v:vs) (Node x ts) = Node x ((ins vs node):others)
  where (nodes, others) = partitionTrieList v ts
        node = head nodes

filterTrie :: (Trie a -> Bool) -> [Trie a] -> Trie a
filterTrie f ts = case (filter f ts) of
  [] -> EmptyTrie
  ts' -> head ts'

findCount :: String -> Counter -> Int
findCount _ EmptyTrie = 0
findCount (x:xs) (Node _ []) = 0
findCount [] (Node v _) = snd v
findCount (x:xs) (Node _ ts) = findCount xs $ f ts
  where f = filterTrie (\(Node v _) -> fst v == x)
findCount (x:xs) (Root ts) = findCount xs $ f ts
  where f = filterTrie (\(Node v _) -> fst v == x)

dispatch :: Counter -> String -> String -> IO (Counter)
dispatch t "add" d = return $ ins d t
dispatch t "find" d = do
    print $ findCount d t
    return t

process :: Counter -> Int -> IO (Counter)
process t i = do
    op_temp <- getLine
    let op_t = words op_temp
    let op = op_t!!0
    let contact = op_t!!1
    dispatch t op contact

main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    foldM_ process trieRoot [1..n]