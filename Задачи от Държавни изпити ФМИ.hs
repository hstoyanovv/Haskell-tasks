--Some functions

quicksort [] = []
quicksort (x:xs) = quicksort less ++ [x] ++ quicksort more
                where less = filter (<= x) xs
                      more = filter (> x) xs

permutations [] = [[]]
permutations lst = [x:p | x <- lst, p <- (permutations (delete x lst))]

delete el [] = []
delete el (x:xs) = if el == x then xs else x : delete el xs

powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

pairs l1 l2 = [(x,y) | x <-l, y <- l]
        where l = l1 ++ l2;


pairs1 = [(x,y) | x <- [0..], y <- [0..x-1]]

numbersN = [(x, y - x) | y <- [2..], x <- [1..(y - 1)]]

powers2 = 1 : map (*2) powers2


--09.2019

addIfNew :: Eq a => a -> [a] -> [a]
addIfNew x l = if x `elem` l then l else x:l

db = [("scheme", [("typing","dynamic"),("evaluation","strict")]),
      ("haskell",[("typing","static")]), ("c++", [])]


evaluation "haskell" = [("evaluation", "lazy")]
evaluation "scheme" = [("evaluation","strict"),("macros", "true")]
evaluation "c++" = evaluation "scheme"

purity :: [Char] -> [([Char], [Char])]
purity lang = if lang == "haskell" then [("pure", "true")] else []


annotate db annotators = 
   map (\(item,labels) -> (item, foldr addIfNew labels (concatMap (\annotator -> annotator item) annotators))) db


--07.2018

addDefault val [] = [val]
addDefault val l = l


sumMinFix fl xl = 
        sum (map (\f -> minimum (addDefault 0 [x | x <- xl, (f x) == x])) fl)

--09.2018

selectList l1 l2 = if length l1 >= length l2 then l1 else l2

sumMaxRoots :: (Eq a1, Num a2, Num a1) => (a2 -> a1) -> [[a2]] -> a2
sumMaxRoots f ll =
    sum (foldr selectList [] (map (\l -> [x | x <-l, f x == 0]) ll))

-- 07.2019
recommended basket bestFit products = 
    filter (\product -> not (product `elem` basket) && findPrice product <= basketCost) (map bestFit basket)
                            where findPrice product = let Just x = lookup product products in x
                                  basketCost = sum (map (\x -> findPrice x) basket)


--09 2015

merge x [] = x
merge [] x = x
merge (x:xs) (y:ys) = if x < y
                        then x : merge xs (y:ys)
                        else y : merge (x:xs) ys

msort [] = []
msort [a] = [a]
msort xs = merge (msort (firstHalf xs)) (msort (secondHalf xs))
            where firstHalf  xs = let { n = length xs } in take (div n 2) xs
                  secondHalf xs = let { n = length xs } in drop (div n 2) xs

--07 2015

--map ​(​head [(\couple­>fst couple + snd couple)]​) (​foldr1 (++) [[(1,2)],[(3,4)]]​)
-- [3,7]

--[zip [x] [x] | x <- [1..5]]
--[[(1,1)],[(2,2)],[(3,3)],[(4,4)],[(5,5)]]

--map (\(x:y:z)­>x:z) [[1,2,3],[2,3,1],[3,1,2]]
-- [[1,3],[2,1],[3,2]]



--09 2014

perms [] = [[]]
perms lst = [x:p | x <- lst, p <- (perms (del x lst))]
            where del el (y:ys) = if el == y then ys
                                    else y : del el ys


--07.2014

--a)
indexByEl n (x:xs) cnt = if n == x then cnt else indexByEl n xs cnt + 1

totalMin fl  = fl !! indexByEl (minimum lst) lst 0
            where lst = [f 0 | f <- fl]


--07.2010

qSort [] = []
qSort (x:xs) = qSort more ++ [x] ++ qSort less
            where less = filter (<= x) xs
                  more = filter (> x) xs

fromLstToInt []  res = res
fromLstToInt (x:xs) res =  fromLstToInt xs (res*10 + x)

findMax [] = 0
findMax lst = fromLstToInt (qSort lst) 0


--09.2010

reverseInt 0  res = res
reverseInt n res = reverseInt (n `div` 10) (res*10 + (n `mod` 10))

toDecimal 0 = 0
toDecimal n = helper 0 n 0
        where helper dec remain iter
                |remain == 0 = dec
                |remain `mod` 10 == 0 = helper dec (remain `div` 10) (iter + 1)
                |otherwise = helper (dec + (2^iter)) ((remain - 1) `div`10) (iter + 1)


toBinary 0 = 0
toBinary n = helper 0 n 0
        where helper bin remain iter
                |remain == 1 = bin + (10^iter)
                |remain `mod` 2 == 0 = helper bin (remain `div` 2) (iter + 1)
                |otherwise = helper (bin + (10^iter)) ((remain - 1) `div`2) (iter + 1)


generateBin :: (Integral a1, Num a2) => a1 -> [a2]
generateBin n = [toBinary x | x <- [n..]]




