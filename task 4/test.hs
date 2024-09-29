-- 1. Функция для вставки пары ключ-значение в Map
data Map k a = EmptyMap
             | Node k a (Map k a) (Map k a)
             deriving (Eq, Show)            -- автоматически создать экземпляры классов типов Eq (позволяет сравнивать =) и Show(преобразование в строку)

-- 1.1 Реализация
myInsert :: Ord k => k -> a -> Map k a -> Map k a
myInsert key value EmptyMap = Node key value EmptyMap EmptyMap
myInsert key value (Node k v left right)
  | key == k  = Node k value left right  -- обновляем значение для существующего ключа
  | key < k   = Node k v (myInsert key value left) right  -- вставляем в левое поддерево
  | key > k   = Node k v left (myInsert key value right)  -- вставляем в правое поддерево


-- ************************************************************************************************
-- 2. Функция, которая создает множество с одним элементом
data Set a = EmptySet
           | Element a (Set a) (Set a)
           deriving (Eq, Show)
-- 2.1 Реализация
singleton :: a -> Set a
singleton x = Element x EmptySet EmptySet

-- ************************************************************************************************
-- 3. Функция, которая возвращает список без первого элемента
myTail :: [a] -> [a]
myTail [] = error "List is empty"  -- Обработка пустого списка
myTail (_:xs) = xs  -- Возвращаем список без первого элемента

-- ************************************************************************************************
-- 4. Функция, которая проверяет, является ли символ цифрой
myIsDigit :: Char -> Bool
myIsDigit ch = ch >= '0' && ch <= '9'

-- ************************************************************************************************
-- 5. Функция, которая проверяет, содержит ли Maybe значение
myIsJust :: Maybe a -> Bool
myIsJust (Just _) = True
myIsJust Nothing  = False

-- **************************************Тестирование**********************************************************
myTest :: (Eq a, Show a) => a -> a -> IO ()
myTest expected actual
  | expected == actual = putStrLn $ "Test passed: " ++ show actual
  | otherwise          = putStrLn $ "Test failed: expected " ++ show expected ++ ", but got " ++ show actual

main :: IO ()
main = do
  print "*****1*****"
  let m = Node 1 "a" (Node 0 "z" EmptyMap EmptyMap) (Node 2 "b" EmptyMap EmptyMap)

  -- Проверяет, что если значение для существующего ключа не изменяется при попытке вставить его заново с тем же значением
  myTest (Node 1 "a" (Node 0 "z" EmptyMap EmptyMap) (Node 2 "b" EmptyMap EmptyMap))
         (myInsert 2 "b" m)
  -- Проверяет, что если вставляется новое значение для существующего ключа, то оно корректно обновляет значение в дереве
  myTest (Node 1 "a" (Node 0 "z" EmptyMap EmptyMap) (Node 2 "c" EmptyMap EmptyMap))
         (myInsert 2 "c" m)
  -- Проверяет, что новый узел добавляется в правильное место в дереве, если его ключ больше, чем ключи всех узлов на пути от корня к узлу
  myTest (Node 1 "a" (Node 0 "z" EmptyMap EmptyMap) (Node 2 "b" EmptyMap (Node 3 "d" EmptyMap EmptyMap)))
       (myInsert 3 "d" m)

  -- Тестирование вставки в множество
  print "*****2*****"
  let s = singleton 2
  myTest (Element 5 EmptySet EmptySet) (singleton 5)
  myTest (Element "hello" EmptySet EmptySet) (singleton "hello")
  myTest (Element True EmptySet EmptySet) (singleton True)
  myTest (Element (-10) EmptySet EmptySet) (singleton (-10))
  myTest (Element "" EmptySet EmptySet) (singleton "")
  myTest (Element 'a' EmptySet EmptySet) (singleton 'a')

   -- Тестирование функции myTail
  print "*****3*****"
  myTest [2, 3, 4] (myTail [1, 2, 3, 4])
  myTest "ello" (myTail "Hello")

   -- Тестирование функции myIsDigit
  print "*****4*****"
  myTest True (myIsDigit '5')
  myTest False (myIsDigit 'a')
  myTest False (myIsDigit '!')
  myTest False (myIsDigit ' ')

    -- Тестирование функции myIsJust
  print "*****5*****"
  myTest True (myIsJust (Just 5))
  myTest False (myIsJust Nothing)

  -- Проверка ошибки для пустого списка
  -- putStrLn "Expecting an error for empty list:"
  -- print (myTail ([] :: [Int]))  -- вызовет ошибку


