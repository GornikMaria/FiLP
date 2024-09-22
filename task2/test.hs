module DeclarationFunctions where

-- 1 oddEven(L)
-- oddEven: функция перестановки местами соседних элементов списка L
-- Примеры:
-- oddEven [2,5,7,9,1,8]  -> [5,2,9,7,8,1]
-- oddEven [1,2,3]        -> [2,1,3]
-- oddEven [42]            -> [42]
-- oddEven []             -> []

oddEven :: [a] -> [a]
-- Если список пустой, возвращаем пустой список
oddEven [] = []

-- Если список состоит из одного элемента, возвращаем его
oddEven [x] = [x]

-- Первые два элемента меняются местами: x:y:xs означает, что список начинается с элемента x, за которым идёт элемент y, а xs — это оставшаяся часть списка
oddEven (x:y:xs) = y:x:oddEven xs

callOddEven:: IO ()
callOddEven = do
    -- Тест 1: Чётный список
    let list = [2, 5, 7, 9, 1, 8]
    let result = oddEven list
    print $ "Original list: " ++ show list
    print $ "Transformed list: " ++ show result
    print "-------------------"

     -- Тест 2: Нечётный список
    let list2 = [1, 2, 3]
    let result2 = oddEven list2
    print $ "Original list: " ++ show list2
    print $ "Transformed list: " ++ show result2
    print "-------------------"

     -- Тест 3: Список с одним элементом
    let list3 = [42]
    let result3 = oddEven list3
    print $ "Original list: " ++ show list3
    print $ "Transformed list: " ++ show result3
    print "-------------------"

    -- Тест 4: Пустой список
    let list4 = ([] :: [Int])
    let result4 = oddEven list4
    print $ "Original list: " ++ show list4
    print $ "Transformed list: " ++ show result4
    print "-------------------"


-- 1 insert (L,A,n)
-- insert: функция включения в список L заданного атома А на определенную позицию n
-- Примеры:
-- insert([2, 5, 7], 9, 2) -> [2, 9, 5, 7]
-- insert([1, 2, 3], 42, 0) -> [42, 1, 2, 3]
-- insert([], 5, 0) -> [5]
-- insert([1, 2], 3, 5) -> [1, 2, 3] (если n больше длины списка, добавляем в конец)

insert :: [a] -> a -> Int -> [a]
-- Если позиция 0, добавляем элемент в начало списка
insert xs a 0 = a : xs

-- Если список пустой, добавляем элемент в него
insert [] a _ = [a]

-- Для всех остальных случаев рекурсивно проходим по списку, пока не дойдём до позиции n
insert (x:xs) a n
    | n > 0 = x : insert xs a (n - 1)  -- Уменьшаем n, пока не дойдём до нужной позиции
    | otherwise = x : xs  -- Если n <= 0, просто возвращаем список

callInsert :: IO ()
callInsert = do
    -- Тест 1: Добавляем элемент на вторую позицию
    let list1 = [2, 5, 7]
    let result1 = insert list1 9 2
    print $ "Original list: " ++ show list1
    print $ "Inserted 9 at position 2: " ++ show result1
    print "-------------------"

    -- Тест 2: Добавляем элемент в начало списка
    let list2 = [1, 2, 3]
    let result2 = insert list2 42 0
    print $ "Original list: " ++ show list2
    print $ "Inserted 42 at position 0: " ++ show result2
    print "-------------------"

    -- Тест 3: Пустой список, добавляем элемент
    let list3 = ([] :: [Int])
    let result3 = insert list3 5 0
    print $ "Original list: " ++ show list3
    print $ "Inserted 5 at position 0: " ++ show result3
    print "-------------------"

    -- Тест 4: Добавляем элемент на позицию больше длины списка
    let list4 = [1, 2]
    let result4 = insert list4 3 5
    print $ "Original list: " ++ show list4
    print $ "Inserted 3 at position 5 (out of bounds): " ++ show result4
    print "-------------------"

-- 3 listSumm(L1, L2)
-- listSumm: Функция, которая складывает элементы двух списков.
-- Возвращает список, состоящий из сумм элементов списков-параметров L1 и L2
-- Если списки разной длины, недостающие элементы считаются нулевыми
-- Примеры:
-- listSumm [1, 2, 3] [4, 5, 6] -> [5, 7, 9]
-- listSumm [1, 2] [3, 4, 5]   -> [4, 6, 5]
-- listSumm [1] [2, 3, 4]      -> [3, 3, 4]
-- listSumm [] [1, 2, 3]       -> [1, 2, 3]

listSumm :: [Int] -> [Int] -> [Int]
-- функция сложения элементов двух списков. Возвращает список , составленный из сумм элементов списков - параметров L1, L2. 
-- Учесть, что переданные списки могут быть разной длины

listSumm xs ys = zipWith (+) xs ys ++ drop (length xs) ys ++ drop (length ys) xs

-- Тестирование функции listSumm
callListSumm :: IO ()
callListSumm = do
    -- Тест 1: Два списка одинаковой длины
    let list1 = [1, 2, 3]
    let list2 = [4, 5, 6]
    let result1 = listSumm list1 list2
    print $ "List 1: " ++ show list1
    print $ "List 2: " ++ show list2
    print $ "Summed list: " ++ show result1
    print "-------------------"

    -- Тест 2: Первый список короче второго
    let list3 = [1, 2]
    let list4 = [3, 4, 5]
    let result2 = listSumm list3 list4
    print $ "List 3: " ++ show list3
    print $ "List 4: " ++ show list4
    print $ "Summed list: " ++ show result2
    print "-------------------"

    -- Тест 3: Второй список короче первого
    let list5 = [1]
    let list6 = [2, 3, 4]
    let result3 = listSumm list5 list6
    print $ "List 5: " ++ show list5
    print $ "List 6: " ++ show list6
    print $ "Summed list: " ++ show result3
    print "-------------------"

    -- Тест 4: Один список пустой
    let list7 = ([] :: [Int])
    let list8 = [1, 2, 3]
    let result4 = listSumm list7 list8
    print $ "List 7: " ++ show list7
    print $ "List 8: " ++ show list8
    print $ "Summed list: " ++ show result4
    print "-------------------"

    -- Тест 5: Оба списка пустые
    let list9 = ([] :: [Int])
    let list10 = ([] :: [Int])
    let result5 = listSumm list9 list10
    print $ "List 9: " ++ show list9
    print $ "List 10: " ++ show list10
    print $ "Summed list: " ++ show result5
    print "-------------------"

-- 4 position(L, A)
-- position: возвращает номер первого вхождения заданного атома  А в список L (Если элемент не найден, возвращает -1)
-- Примеры:
-- position [2, 5, 7, 9, 1, 8] 5 -> 1
-- position [2, 5, 7, 9, 1, 8] 9 -> 3
-- position [2, 5, 7, 9, 1, 8] 10 -> -1
-- position [] 1 -> -1

position :: Eq a => [a] -> a -> Int
position xs a = findPosition xs a 0
  where
    findPosition [] _ _ = -1
    findPosition (x:xs) a index
      | x == a    = index
      | otherwise = findPosition xs a (index + 1)

-- Тестирование функции position
callPosition :: IO ()
callPosition = do
    -- Тест 1: Элемент присутствует в списке
    let list1 = [2, 5, 7, 9, 1, 8]
    let element1 = 5
    let result1 = position list1 element1
    print $ "List 1: " ++ show list1
    print $ "Element: " ++ show element1
    print $ "Position: " ++ show result1
    print "-------------------"

    -- Тест 2: Элемент присутствует в конце списка
    let list2 = [2, 5, 7, 9, 1, 8]
    let element2 = 8
    let result2 = position list2 element2
    print $ "List 2: " ++ show list2
    print $ "Element: " ++ show element2
    print $ "Position: " ++ show result2
    print "-------------------"

    -- Тест 3: Элемент отсутствует в списке
    let list3 = [2, 5, 7, 9, 1, 8]
    let element3 = 10
    let result3 = position list3 element3
    print $ "List 3: " ++ show list3
    print $ "Element: " ++ show element3
    print $ "Position: " ++ show result3
    print "-------------------"

    -- Тест 4: Пустой список
    let list4 = ([] :: [Int])
    let element4 = 1
    let result4 = position list4 element4
    print $ "List 4: " ++ show list4
    print $ "Element: " ++ show element4
    print $ "Position: " ++ show result4
    print "-------------------"

-- 5 sumF(n)
-- sumF: Функция, которая вычисляет сумму первых n натуральных чисел
-- Используется формула: F(n) = n * (n + 1) // 2
-- Примеры:
-- sumF 1  -> 1
-- sumF 5  -> 15
-- sumF 10 -> 55

sumF :: Int -> Int
sumF n = n * (n + 1) `div` 2

callSumF :: IO ()
callSumF = do
    -- Тест 1: n = 1
    let n1 = 1
    let result1 = sumF n1
    print $ "Sum of first " ++ show n1 ++ " numbers: " ++ show result1
    print "-------------------"

    -- Тест 2: n = 5
    let n2 = 5
    let result2 = sumF n2
    print $ "Sum of first " ++ show n2 ++ " numbers: " ++ show result2
    print "-------------------"

    -- Тест 3: n = 20
    let n3 = 10
    let result3 = sumF n3
    print $ "Sum of first " ++ show n3 ++ " numbers: " ++ show result3
    print "-------------------"

    -- Тест 4: n = 0
    let n4 = 0
    let result4 = sumF n4
    print $ "Sum of first " ++ show n4 ++ " numbers: " ++ show result4
    print "-------------------"

-- 6 sumF2(n)
-- sumF2: Функция, которая вычисляет сумму F(n) = ∑{i=1}^{n}(n - i)
-- Используется упрощенная формула: F(n) = n^2 - (n * (n + 1) // 2)
-- Примеры:
-- sumF2 1  -> 0
-- sumF2 5  -> 10
-- sumF2 10 -> 45

sumF2 :: Int -> Int
sumF2 n = n * n - (n * (n + 1) `div` 2)

-- Тестирование функции sumF2
callSumF2 :: IO ()
callSumF2 = do
    -- Тест 1: n = 1
    let n1 = 1
    let result1 = sumF2 n1
    print $ "Sum F(" ++ show n1 ++ ") = " ++ show result1
    print "-------------------"

    -- Тест 2: n = 5
    let n2 = 5
    let result2 = sumF2 n2
    print $ "Sum F(" ++ show n2 ++ ") = " ++ show result2
    print "-------------------"

    -- Тест 3: n = 10
    let n3 = 10
    let result3 = sumF2 n3
    print $ "Sum F(" ++ show n3 ++ ") = " ++ show result3
    print "-------------------"

    -- Тест 4: n = 0
    let n4 = 0
    let result4 = sumF2 n4
    print $ "Sum F(" ++ show n4 ++ ") = " ++ show result4
    print "-------------------"

main :: IO ()
main = do
    print $ "task 1"
    callOddEven
    print $ "task 2"
    callInsert
    print $ "task 3"
    callListSumm
    print $ "task 4"
    callPosition
    print $ "task 5"
    callSumF
    print $ "task 6"
    callSumF2
