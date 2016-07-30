--f :: [[Int]] -> [(Integer,[Char])] -> [String]
--f votos nombres
--    | length ganador >0 = [] -- TODO 
--    | otherwise = f [[]] nombres
--    where
--        v = [] ----sort . tally $ map head l
--        pref = prefQuitar v ---(snd . head v)
--        recont = reconteo pref votos
--        ganador = criterio v maxvotos
--        maxvotos = partEntera ky
--        ky = fromIntegral $ 1.0 + length v / 2.0

--------------------
--Ordena los votos iniciales por cabeza
--insertSort::Ord a =>[a] ->[a]
--insertSort [] = []
--insertSort (x:xs) = insert x (insertSort xs)

---Si existe votos n>=50 retorna n>=0 sino -1
--Example input criterio [[1,2],[3,3]] 3.0 -> ouput [3] 
criterio:: [[Int]] -> Double -> [Int]
criterio [] _ = []
criterio ([x1,x2]:xs) freq
    | ((fromIntegral x2)::Double) >= freq = x1 : criterio xs freq
    | otherwise = criterio xs freq
--------------------------------------------
---función que saca de la lista la cabeza

-------------------------------------------------------------------------------------
--función parte entera
--Example input partEntera 1.2345 -> ouput 1
partEntera :: (RealFloat a) => a -> Int
partEntera x 
    | x == 0    = 0
    |otherwise =  truncate (x)
-----------------------------------------------------------------------------------------
--función que asocia nombres con respectivos números de voto 1<n<=20.-------
--Example input tally [1,2,3,4] -> [(1,1),(2,2),(3,3),(4,4)]
emparejar:: [a] -> [(Integer, a)]
emparejar [] = []
emparejar y = zip [1..20] y 

--Example input tally [1,2,3,4] -> [(1,1),(2,1),(3,1),(4,1)]
tally :: [Int] -> [(Int, Int)] 
tally [] = []
tally l@(x:xs) = (x, freq) : tally nx
    where
        freq = length l - length nx 
        nx = dropWhile (x==) l

--Example input reconteo [1,2] [[1,2],[2,3],[2,1]] -> ouput [[2],[3],[1]]
reconteo :: [Int] -> [[Int]] -> [[Int]]
reconteo _ [] = []
reconteo [] ls=ls
reconteo pref (l:ls)
    |head l `elem` pref = tail l : reconteo pref ls
    |otherwise = l: reconteo pref ls

--Menores empatados para eliminar:
--Example input prefQuitar [[1,2],[1,2]] 1 -> ouput [2,2]
prefQuitar :: [[Int]] -> Int -> [Int]
prefQuitar [] _ = []
prefQuitar ([x1,x2]:xs) freq
    | x1 == freq = x2 : prefQuitar xs freq
    |otherwise =[]