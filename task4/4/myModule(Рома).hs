-- myLookup - поиск элемента по ключу в Map
data Map k a = EmptyMap | Node k a (Map k a) (Map k a)
  deriving (Show, Eq)
 
myLookup :: (Ord k) => k -> Map k a -> Maybe a
myLookup _ EmptyMap = Nothing
myLookup key (Node k v left right)
  | key < k = myLookup key left
  | key > k = myLookup key right
  | otherwise = Just v
 
-- myMember - проверка наличия элемента в Set
data Set a = EmptySet | Element a (Set a) (Set a)
  deriving (Show, Eq)
 
myMember :: (Ord a) => a -> Set a -> Bool
myMember _ EmptySet = False
myMember x (Element e left right)
  | x < e = myMember x left
  | x > e = myMember x right
  | otherwise = True
 
-- myHead - безопасное получение первого элемента списка
myHead :: [a] -> Maybe a
myHead []    = Nothing
myHead (x:_) = Just x
 
-- myToUpper - преобразование символа в верхний регистр
myToUpper :: Char -> Char
myToUpper c
  | c >= 'a' && c <= 'z' = toEnum (fromEnum c - 32)
  | otherwise            = c
 
-- myFromMaybe - возвращает значение Maybe, если оно есть, иначе - значение по умолчанию
myFromMaybeOrNil :: a -> Maybe a -> a
myFromMaybeOrNil def Nothing  = def -- значение по умолчанию
myFromMaybeOrNil _ (Just val) = val
 
test :: (Eq a, Show a) => a -> a -> IO ()
test expected actual
  | expected == actual = putStrLn $ "Test passed: " ++ show actual
  | otherwise          = putStrLn $ "Test failed: expected " ++ show expected ++ ", but got " ++ show actual
 
main :: IO ()
main = do
  let m = Node 1 "a" (Node 0 "z" EmptyMap EmptyMap) (Node 2 "b" EmptyMap EmptyMap)
  
  putStr "Testing myLookup() on data: "
  print m
  
  test (Just "a") (myLookup 1 m)
  test (Just "b") (myLookup 2 m)
  test (Just "z") (myLookup 0 m)
  test Nothing (myLookup 4 m)
  
  
  let s = Element 2 (Element 1 EmptySet EmptySet) (Element 3 EmptySet EmptySet)
  putStr "\nTesting myMember() on data: "
  print s
 
  test True (myMember 2 s)
  test True (myMember 1 s)
  test True (myMember 3 s)
  test False (myMember 4 s)
  
  putStrLn "\nTesting myHead()"
  test (Just 1) (myHead [1, 2, 3])
  test (Just 2) (myHead [2])
  test (Nothing :: Maybe Int) (myHead [])
  
  putStrLn "\nTesting myToUpper()"
  test 'A' (myToUpper 'a')
  test 'B' (myToUpper 'B')
  test '1' (myToUpper '1')
  
  putStrLn "\nTesting myFromMaybeOrNil()"
  test 5 (myFromMaybeOrNil 0 (Just 5))
  test 0 (myFromMaybeOrNil 0 Nothing)
  test "Y" (myFromMaybeOrNil "Y" (myHead ["Y", "N"]))


Testing myLookup() on data: Node 1 "a" (Node 0 "z" EmptyMap EmptyMap) (Node 2 "b" EmptyMap EmptyMap)
Test passed: Just "a"
Test passed: Just "b"
Test passed: Just "z"
Test passed: Nothing

Testing myMember() on data: Element 2 (Element 1 EmptySet EmptySet) (Element 3 EmptySet EmptySet)
Test passed: True
Test passed: True
Test passed: True
Test passed: False

Testing myHead()
Test passed: Just 1
Test passed: Just 2
Test passed: Nothing

Testing myToUpper()
Test passed: 'A'
Test passed: 'B'
Test passed: '1'

Testing myFromMaybeOrNil()
Test passed: 5
Test passed: 0
Test passed: "Y"