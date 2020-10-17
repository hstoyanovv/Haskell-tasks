
--testGraph2 :: Graph
--testGraph2 = [[1,2],[2,3],[3,1,4],[4,2]]

--maxCycle :: Graph -> Int -> [Int]
--maxCycle g v = maxLength [ p++[v] | p<-allPaths g, v == head p, v `elem` (neighbours (last p) g) ]

import Data.List (maximumBy, minimumBy)
import Data.Ord (comparing)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving(Eq, Ord, Show, Read)

takeconcat1 :: Tree Int -> [Int]
takeconcat1 Empty = []
takeconcat1 (Node val Empty Empty) = [val]
takeconcat1 (Node val l r) = [val] ++ (takeconcat1 l) ++ (takeconcat1 r)

intervalTree1 :: Tree Int -> Tree [Int]
intervalTree1 Empty = Empty
intervalTree1 (Node val Empty Empty) = (Node [val,val] Empty Empty)
intervalTree1 (Node val l r) = (Node [minimum subtree, maximum subtree] (intervalTree1 l) (intervalTree1 r))
                                       where subtree = takeconcat1 (Node val l r)


t1 :: Tree Int
t1 = Node 1 (Node 2 (Node 5 Empty Empty) (Node 4 Empty Empty)) (Node 3 Empty (Node 7 (Node 8 Empty Empty) Empty))

videoclips = [("lolcat", 15), ("dogewow", 35), ("omgseethis", 28)]


averageVideo :: [(String,Int)] -> String
averageVideo lst = fst (maximumBy (comparing snd) (filter (\x -> snd x <= avg) lst))
                                                  where avg = (sum [x | x <- (map snd lst)]) `quot` (length lst)




takt (n,_,_) = n
razmer (_,a,_) = a
rythm (_,_,b) = b

tempo :: Fractional a => (a,a,a) -> a
tempo segment = (takt segment * razmer segment) / rythm segment

duration :: Fractional a => [(a,a,a)] -> a
duration lst = (sum [ (tempo x) | x <- lst])

maxTempo :: (Ord a, Fractional a) => [(a,a,a)] -> a
maxTempo lst = maximum (map tempo lst)

maxSegments :: (Fractional a, Ord a) => [(a,a,a)] -> [(a,a,a)]
maxSegments lst = [i | i <- lst, (tempo i == maxTempo lst)]

longestSlowest :: (Ord a, Fractional a) => [(a,a,a)] -> (a,a,a)
longestSlowest lst = (minEl (maxSegments lst) (head (maxSegments lst)))

minEl :: (Ord a, Fractional a) => [(a,a,a)] -> (a,a,a) -> (a,a,a)
minEl [] res = res
minEl (x:xs) res
      |udari x < udari res = minEl xs x
      |otherwise = minEl xs res
      where udari a = takt a * razmer a


findMiddle :: (Ord a, Fractional a) => [(a,a,a)] -> a
findMiddle lst = helper lst 0
                  where helper (x:xs) result
                          |tempo x + result >= avg = avg
                          |otherwise = helper xs (result + tempo x)
                          where avg = (duration lst) / 2

---------------
otherlst :: [[Int]] -> [(Int -> Int)] -> [[Int]]
otherlst [] [] = []
otherlst l1 l2 = (map (head l2) (head l1)) : (otherlst (tail l1) (tail l2))

all1 :: (a -> Bool) -> [a] -> Bool
all1 p l = and (map p l)

position :: [[Int]] -> Int
position l = helper (head l)
            where helper [] = 0
                  helper l1
                    |(all1 (\x -> (head l1) `elem` x) (tail l)) = head l1
                    |otherwise = helper (tail l1)


positions :: [[Int]] -> Int -> [Int]
positions l n = map (\x -> (positionnumber x n 0)) l
                             where positionnumber l n cnt
                                     |(head l) == n = cnt
                                     |otherwise = positionnumber (tail l) n (cnt + 1)

kur :: [[Int]] -> [Int] -> [Int]
kur [] [] = []
kur l1 l2 = ((head l1) !! (head l2)) : kur (tail l1) (tail l2)


allEqual :: [[Int]] -> [(Int -> Int)] -> [Int]
allEqual [] [] = []
allEqual l1 l2 = kur l1 (positions (otherlst l1 l2) (position (otherlst l1 l2)))


---zad4


l = [("A",[("p",6),("q",9)]),("B",[("p",2),("q",3)]),("C",[("p",3)])]



type Ingredient = (String, Integer)
type Medicine = (String, [Ingredient])
          
takeSnds :: [(String, Integer)]->[Integer]
takeSnds [] = []
takeSnds lst = map snd lst

takeFsts :: [(String, Integer)] -> [String]
takeFsts [] = []
takeFsts lst = map fst lst

all3 p lst = and (map p lst)
isSubstitute :: Medicine -> Medicine -> Bool
isSubstitute ( _ , (a:as)) ( _ , (b:bs))
     |(lek1 == lek2) && (all3 (\x -> x `elem` (takeFsts (b:bs))) (takeFsts (a:as))) && (length (a:as) == (length (b:bs))) = True
     |otherwise = False
     where lek1 = foldr (quot) (snd a) (takeSnds as)
           lek2 = foldr (quot) (snd b) (takeSnds bs)
    

bestSubstitute :: Medicine -> [Medicine] -> Medicine
bestSubstitute med meds = maximumBy (comparing snd) [i | i<-meds, snd i <= snd med]
    

length1 :: Num a => [a] -> a
length1 [] = 0
length1 (x:xs) = 1 + length1 xs

--task plants
type Plant = (String,Int,Int)
plName :: Plant -> String
plName (a,_,_) = a
plMin :: Plant -> Int
plMin(_,b,_) = b
plMax :: Plant -> Int
plMax (_,_,c) = c

allIntervals :: [Plant] -> [(Int,Int)]
allIntervals plants = [(minT,maxT) | minT <- (map plMin plants), maxT <- (map plMax plants), minT < maxT]

getNames :: (Int,Int) -> [Plant] -> [String]
getNames int plants = [plName p | p <- plants, growsIn int p]
         where growsIn (minT,maxT) (_,plMin,plMax) = plMin <= minT && maxT <= plMax

garden :: [Plant] -> ((Int,Int),[String])
garden plants = maximumBy (comparing (length . snd)) [ (int, getNames int plants) | int<- (allIntervals plants)]



