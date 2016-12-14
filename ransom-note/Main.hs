import Control.Applicative
import Control.Monad
import System.IO
import qualified Data.Map.Lazy as M
import Data.List
import Data.Maybe

increment :: M.Map String Int -> String -> M.Map String Int
increment m s = case (M.lookup s m) of
                  Nothing -> M.insert s 0 m
                  Just c  -> M.insert s (c+1) m
                  
decrement :: Maybe (M.Map String Int) -> String -> Maybe (M.Map String Int)
decrement m s = do
  m' <- m
  v <- M.lookup s m'
  let decremented = M.insert s (v-1) m'
  if v < 0
    then Nothing
    else return decremented

memoize :: [String] -> M.Map String Int
memoize ws = foldl' increment M.empty ws

canUse :: M.Map String Int -> [String] -> String
canUse mag ws = case (foldl' decrement (Just mag) ws) of
    Nothing -> "No"
    _ -> "Yes"

main :: IO ()
main = do
    m_temp <- getLine
    let m_t = words m_temp
    let m = read $ m_t!!0 :: Int
    let n = read $ m_t!!1 :: Int
    magazine_temp <- getLine
    let magazine = words magazine_temp
    ransom_temp <- getLine
    let ransom = words ransom_temp
    let mMap = memoize magazine
    putStrLn $ canUse mMap ransom