import Math.KMeans
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))

import Debug.Trace
traceS a = trace $ show a

type Vec = V.Vector Double

main = do
  cs <- readFile "iris.data"
  let ls = lines cs
  let xss = [take 4 seps | seps<-map (sepBy ',') ls, seps/=[""]]
  let vecs = traceS xss $ readVecs xss
  let cls = kmeans 3 vecs
  mapM_ (printCluster cls) [0..2]

printCluster :: [[Vec]] -> Int -> IO ()
printCluster cls i =
  mapM_ (printVector i) cluster
  where cluster = cls !! i
        printVector :: Int -> Vec -> IO ()
        printVector i vec = 
         putStrLn $ show (vec ! 0)++" "++show (vec ! 1)++" "++show (fromIntegral i/2.0)
{-
  where go dim nb k 0 = return ()
        go dim nb k i = do
          vectors <- generateVecs (read nb :: Int) (read dim :: Int)
          let cls = kmeans (read k :: Int) vectors
          putStrLn . show $ cls
          go dim nb k (i-1)
-}

readVecs :: [[String]] -> [Vec]
readVecs xss = map (V.fromList.(map read)) xss

sepBy :: Eq a => a -> [a] -> [[a]]
sepBy key str = sepBy' key str
  where
    sepBy' k [] = [[]]
    sepBy' k (c:cs) 
      | k == c = [] : sepBy' k cs
      | otherwise =
        case sepBy' key cs of
          [] -> [[c]]
          (s:ss) -> (c:s):ss
