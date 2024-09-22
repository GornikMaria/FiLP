import Data.Maybe (catMaybes)
-- 1 listnums
-- listnums: берет численный аргумент n  и возвращает список всех чисел от n до 1, включительно
-- Примеры:
-- listNums 5  -> [5,4,3,2,1]
-- listNums 1  -> [1]
-- listNums 0  -> []
-- listNums (-3) -> []

listNums :: Int -> [Int]
listNums n
  | n <= 0    = []             -- Если n меньше или равно 0, возвращаем пустой список
  | otherwise = [n, n-1 .. 1]  -- Генерируем список от n до 1

callListNums :: IO ()
callListNums = do
    -- Тест 1: n = 5
    let n1 = 5
    let result1 = listNums n1
    print $ "List of numbers from " ++ show n1 ++ " to 1: " ++ show result1
    print "-------------------"

    -- Тест 2: n = 1
    let n2 = 1
    let result2 = listNums n2
    print $ "List of numbers from " ++ show n2 ++ " to 1: " ++ show result2
    print "-------------------"

    -- Тест 3: n = 0
    let n3 = 0
    let result3 = listNums n3
    print $ "List of numbers from " ++ show n3 ++ " to 1: " ++ show result3
    print "-------------------"

    -- Тест 4: n = -3
    let n4 = -3
    let result4 = listNums n4
    print $ "List of numbers from " ++ show n4 ++ " to 1: " ++ show result4
    print "-------------------"

-- 2 secondlastlist
-- secondlastlist: функция берет список  списков и возвращает последние элементы каждого, объединенные  в список
-- Примеры:
-- secondLastList [[1,2,3], [4,5], [6]]    -> [3, 5, 6]
-- secondLastList [[10,20], [30], [40,50]] -> [20, 30, 50]
-- secondLastList [[]]                     -> []

secondLastList :: [[a]] -> [a]
secondLastList = catMaybes . map lastButOne
  where
    lastButOne :: [a] -> Maybe a
    lastButOne [] = Nothing              -- Возвращаем Nothing для пустого списка
    lastButOne [x] = Just x              -- Возвращаем единственный элемент в Just
    lastButOne xs = Just (last xs)       -- Возвращаем последний элемент в Just

callSecondLastList :: IO ()
callSecondLastList = do
    -- Тест 1: Списки с разными элементами
    let list1 = [[1, 2, 3], [4, 5], [6]]
    let result1 = secondLastList list1
    print $ "Last elements from " ++ show list1 ++ ": " ++ show result1
    print "-------------------"

    -- Тест 2: Списки с двумя элементами
    let list2 = [[10, 20], [30], [40, 50]]
    let result2 = secondLastList list2
    print $ "Last elements from " ++ show list2 ++ ": " ++ show result2
    print "-------------------"

    -- Тест 3: Списки с пустыми и непустыми элементами
    let list3 = [[], [1, 2, 3], [4]]
    let result3 = secondLastList list3
    print $ "Last elements from " ++ show list3 ++ ": " ++ show result3
    print "-------------------"

    -- Тест 4: Пустой список
    let list4 = ([] :: [[Int]])
    let result4 = secondLastList list4
    print $ "Last elements from " ++ show list4 ++ ": " ++ show result4
    print "-------------------"

-- 3 myunion
-- myunion: функция, которая находит объединение двух  списков. 
-- Объединением двух списков будет список содержащий элементы,  которые есть по крайней мере в одном из списков
-- Примеры:
-- myunion [1, 2, 3] [2, 3, 4] -> [1, 2, 3, 4]
-- myunion [5, 6] []           -> [5, 6]
-- myunion [] [7, 8, 9]        -> [7, 8, 9]
-- myunion [] []               -> []

myunion :: Eq a => [a] -> [a] -> [a]  -- поддержка сравнения на равенство
myunion xs ys = xs ++ [y | y <- ys, y `notElem` xs]  -- xs ++ перебор элементов y элементов (y | y <- ys) из второго списка, которых нет в xs

callMyUnion :: IO ()
callMyUnion = do
    -- Тест 1: Объединение списков с общими элементами
    let list1 = [1, 2, 3]
    let list2 = [2, 3, 4]
    let result1 = myunion list1 list2
    print $ "Union of " ++ show list1 ++ " and " ++ show list2 ++ " is " ++ show result1
    print "-------------------"

    -- Тест 2: Один список пуст
    let list3 = [5, 6]
    let list4 = ([] :: [Int])
    let result2 = myunion list3 list4
    print $ "Union of " ++ show list3 ++ " and " ++ show list4 ++ " is " ++ show result2
    print "-------------------"

    -- Тест 3: Оба списка пусты
    let list5 = ([] :: [Int])
    let list6 = ([] :: [Int])
    let result3 = myunion list5 list6
    print $ "Union of " ++ show list5 ++ " and " ++ show list6 ++ " is " ++ show result3
    print "-------------------"

    -- Тест 4: Объединение списков без общих элементов
    let list7 = [7, 8]
    let list8 = [9, 10]
    let result4 = myunion list7 list8
    print $ "Union of " ++ show list7 ++ " and " ++ show list8 ++ " is " ++ show result4
    print "-------------------"

-- 4 mysubst
-- mysubst: Получив два списка, она возвращает их разность. Разность двух списков называется список, состоящий из элементов  первого списка, которые не принадлежат второму списку
-- Примеры:
-- mysubst [1, 2, 3] [2, 3, 4] -> [1]
-- mysubst [5, 6, 7] [5, 8, 9] -> [6, 7]
-- mysubst [] [1, 2, 3]       -> []
-- mysubst [1, 2, 3] []       -> [1, 2, 3]

mysubst :: Eq a => [a] -> [a] -> [a]
mysubst xs ys = [x | x <- xs, x `notElem` ys]

callMySubst :: IO ()
callMySubst = do
    -- Тест 1: Обычные списки
    let list1 = [1, 2, 3]
    let list2 = [2, 3, 4]
    let result1 = mysubst list1 list2
    print $ "Difference of " ++ show list1 ++ " and " ++ show list2 ++ ": " ++ show result1
    print "-------------------"

    -- Тест 2: Списки с разными элементами
    let list3 = [5, 6, 7]
    let list4 = [5, 8, 9]
    let result2 = mysubst list3 list4
    print $ "Difference of " ++ show list3 ++ " and " ++ show list4 ++ ": " ++ show result2
    print "-------------------"

    -- Тест 3: Пустой первый список
    let list5 = ([] :: [Int])
    let list6 = [1, 2, 3]
    let result3 = mysubst list5 list6
    print $ "Difference of " ++ show list5 ++ " and " ++ show list6 ++ ": " ++ show result3
    print "-------------------"

    -- Тест 4: Пустой второй список
    let list7 = [1, 2, 3]
    let list8 = ([] :: [Int])
    let result4 = mysubst list7 list8
    print $ "Difference of " ++ show list7 ++ " and " ++ show list8 ++ ": " ++ show result4
    print "-------------------"

-- 5 nposlist
-- nposlist: берущую список списков и возвращающую список из N -х элементов подсписков с помощью функций map и (!!)
-- Примеры:
-- nposlist [[1, 2, 3], [4, 5, 6], [7, 8]] 2 -> [3, 6, 8]
-- nposlist [[10, 20], [30], [40, 50]] 1     -> [20, 30, 50]
-- nposlist [[1, 2], [3, 4, 5]] 0            -> [1, 3]
-- nposlist [[]] 0                           -> []

nposlist :: Int -> [[a]] -> [a]
nposlist n = catMaybes . map (\xs -> if length xs > n then Just (xs !! n) else Nothing)
-- подсписок xs, xs !! n берет элемент с данным индексом

callNposList :: IO ()
callNposList = do
    -- Тест 1: Списки с разными элементами
    let list1 = [[1, 2, 3], [4, 5, 6], [7, 8]]
    let result1 = nposlist 2 list1
    print $ "N-th elements from " ++ show list1 ++ " (N=2): " ++ show result1
    print "-------------------"

    -- Тест 2: Списки с одним элементом
    let list2 = [[10, 20], [30], [40, 50]]
    let result2 = nposlist 1 list2
    print $ "N-th elements from " ++ show list2 ++ " (N=1): " ++ show result2
    print "-------------------"

    -- Тест 3: Нулевой индекс
    let list3 = [[1, 2], [3, 4, 5]]
    let result3 = nposlist 0 list3
    print $ "N-th elements from " ++ show list3 ++ " (N=0): " ++ show result3
    print "-------------------"

    -- Тест 4: Пустой список
    let list4 = ([] :: [[Int]])
    let result4 = nposlist 0 list4
    print $ "N-th elements from " ++ show list4 ++ " (N=0): " ++ show result4
    print "-------------------"

main :: IO ()
main = do
    print $ "task 1"
    callListNums
    print "*****************"
    print $ "task 2"
    callSecondLastList
    print "*****************"
    print $ "task 3"
    callMyUnion
    print "*****************"
    print $ "task 4"
    callMySubst
    print "*****************"
    print $ "task 5"
    callNposList
    print "*****************"