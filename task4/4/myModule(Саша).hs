data Set a = Empty
           | Node a (Set a) (Set a)
           deriving (Show)

data Map k v = EmptyMap
             | NodeMap k v (Map k v) (Map k v)
             deriving (Show)

insertSet :: Ord a => a -> Set a -> Set a
insertSet x Empty = Node x Empty Empty
insertSet x (Node y left right)
    | x < y     = Node y (insertSet x left) right
    | x > y     = Node y left (insertSet x right)
    | otherwise = Node y left right  -- Если элемент уже существует, не добавляем его снова

insertMap :: Ord k => k -> v -> Map k v -> Map k v
insertMap k v EmptyMap = NodeMap k v EmptyMap EmptyMap
insertMap k v (NodeMap key value left right)
    | k < key   = NodeMap key value (insertMap k v left) right
    | k > key   = NodeMap key value left (insertMap k v right)
    | otherwise = NodeMap key v left right  -- Если ключ уже существует, обновляем значение



-- 1. Data.Map  size :: Map k a -> Int Возвращает количество элементов в Map
mySizeSet :: Set a -> Int
mySizeSet Empty = 0
mySizeSet (Node _ left right) = 1 + mySizeSet left + mySizeSet right  --функция возвращает 1 (это сам узел) плюс рекурсивный подсчет элементов в левом и правом поддеревьях

mySizeSetTests :: IO()
mySizeSetTests = do
    putStrLn $ "mySizeSet tests:"
    let mySet1 = insertSet 3 $ insertSet 1 $ insertSet 2 Empty
    print $ mySizeSet mySet1  -- Вывод: 3

    let mySet2 = insertSet 'a' $ insertSet 'b' Empty
    print $ mySizeSet mySet2  -- Вывод: 2

    let mySet3 = Empty
    print $ mySizeSet mySet3  -- Вывод: 0

    putStrLn $ ""


-- 2. Data.Set  size :: Set a -> Int Возвращает количество элементов в Set
mySizeMap :: Map k v -> Int
mySizeMap EmptyMap = 0
mySizeMap (NodeMap _ _ left right) = 1 + mySizeMap left + mySizeMap right

mySizeMapTests :: IO()
mySizeMapTests = do
    putStrLn $ "mySizeMap tests:"
    let myMap1 = insertMap 1 1 $ insertMap 2 2 $ insertMap 3 3 EmptyMap
    print $ mySizeMap myMap1  -- Вывод: 3

    let myMap2 = insertMap 1 'a' $ insertMap 2 'b' EmptyMap
    print $ mySizeMap myMap2  -- Вывод: 2

    let myMap3 = EmptyMap
    print $ mySizeMap myMap3  -- Вывод: 0

    putStrLn $ ""


-- 3. Data.List length :: [a] -> Int Возвращает длину списка
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

myLengthTests :: IO()
myLengthTests = do
    putStrLn $ "myLength tests:"
    let myList1 = [1, 2, 3, 4, 5]
    print $ myLength myList1  -- Вывод: 5

    let myList2 = ['a', 'b', 'c']
    print $ myLength myList2  -- Вывод: 3

    let myList3 = []
    print $ myLength myList3  -- Вывод: 0

    putStrLn $ ""

-- 4. Data.Char isAlpha :: Char -> Bool Проверяет, является ли символ буквой
myIsAlpha :: Char -> Bool
myIsAlpha c = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || ('а' <= c && c <= 'я') || ('А' <= c && c <= 'Я')
 
myIsAlphaTests :: IO()
myIsAlphaTests = do
    putStrLn $ "myIsAlpha tests:"

    print $ myIsAlpha 'a'  -- Вывод: True
    print $ myIsAlpha 'G'  -- Вывод: True
    print $ myIsAlpha 'а'  -- Вывод: True
    print $ myIsAlpha 'В'  -- Вывод: True
    print $ myIsAlpha '1'  -- Вывод: False
    print $ myIsAlpha '%'  -- Вывод: False
    print $ myIsAlpha ' '  -- Вывод: False

    putStrLn $ ""

-- 5. Data.Maybe catMaybes :: [Maybe a] -> [a] Собирает все значения из списка Maybe (внутри конструкций Just)
myCatMaybes :: [Maybe a] -> [a]
myCatMaybes [] = []
myCatMaybes (Nothing:xs) = myCatMaybes xs  -- Если первый элемент списка — Nothing, пропускаем его и рекурсивно обрабатываем оставшуюся часть списка xs
myCatMaybes (Just x:xs) = x : myCatMaybes xs -- Если первый элемент — Just x, добавляем x в новый список и продолжаем обработку оставшейся части xs

myCatMaybesTests :: IO()
myCatMaybesTests = do
    putStrLn $ "myCatMaybes tests:"

    let listOfMaybes1 = [Just 1, Nothing, Just 2, Nothing, Just 3]
    print $ myCatMaybes listOfMaybes1  -- Вывод: [1, 2, 3]

    let listOfMaybes2 = [Just 'a', Nothing]
    print $ myCatMaybes listOfMaybes2  -- Вывод: ["a"]

    let listOfMaybes3 = [Nothing] :: [Maybe Int]
    print $ myCatMaybes listOfMaybes3  -- Вывод: []

    putStrLn $ ""

main :: IO ()
main = do
    mySizeSetTests
    mySizeMapTests
    myLengthTests
    myIsAlphaTests
    myCatMaybesTests