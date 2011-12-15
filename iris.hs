import Math.KMeans (kmeans)
import qualified Data.Vector.Unboxed as V
import Data.Vector.Unboxed ((!))
import qualified Data.Map as Map

import Debug.Trace
traceS a = trace $ show a

type Vec = V.Vector Double

main = do
  cs <- readFile "iris.data"
  let ls = lines cs
  let sepss = [seps | seps<-map (sepBy ',') ls, seps/=[""]]
  let xss = map (take 4) sepss
  let irisMap = Map.fromList $ map (\seps->(readVec $ take 4 seps, last seps)) sepss
  let vecs = readVecs xss
  let cls = kmeans 3 vecs
  let (correct,all) = (correctNum cls irisMap, sum $ map length cls)
  putStrLn $ "Correct/All=" ++ show correct ++ "/" ++ show all
  putStrLn $ "Accuracy=" ++ show (fromIntegral correct / fromIntegral all)
  putStrLn "Putting clusters into iris.cls[0-2]"
  mapM_ (writeCluster cls irisMap) [0..2]

correctNum :: [[Vec]] -> Map.Map Vec String -> Int
correctNum cls irisMap = 
  (length $ filter (\vec->(Map.!) irisMap vec == "Iris-setosa") (cls!!0)) +
  (length $ filter (\vec->(Map.!) irisMap vec == "Iris-versicolor") (cls!!1)) +
  (length $ filter (\vec->(Map.!) irisMap vec == "Iris-virginica") (cls!!2))


writeCluster :: [[Vec]] -> Map.Map Vec String -> Int -> IO ()
writeCluster cls irisMap i =
  writeFile ("iris.cls"++show i) c
  where cluster = cls !! i
        c = unlines $ map showElement cluster
        showElement vec = 
          show (vec!0) ++ " " ++ show (vec!1) ++ " " ++ 
          show (vec!2) ++ " " ++ show (vec!3) ++ " " ++ (Map.!) irisMap vec

--showElement vec = show vec--show (vec ! 0)++" "++show (vec ! 1)

readVecs :: [[String]] -> [Vec]
readVecs xss = map readVec xss

readVec = V.fromList.(map read)

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
