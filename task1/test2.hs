-- head возвращает первый элемент списка
-- tail возвращает все элементы списка, кроме первого
main :: IO ()
main = do
    -- 1) Получить элемент 'b' из списка ['a', 'b', 'c']

    let list1 = ['a', 'b', 'c']
    -- 'b' является вторым элементом списка, используем tail для удаления 'a' и head для получения 'b'
    let element1 = head (tail list1)
    putStrLn $ "Element from list1: " ++ [element1]
    
    -- 2) Получить элемент 'b' из списка [['a', 'b'], ['c', 'd']]
    let list2 = [['a', 'b'], ['c', 'd']]
    let element2 = head (tail (head list2))
    putStrLn $ "Element from list2: " ++ [element2]
    
    -- 3) Получить элемент 'b' из списка [['a', 'c', 'd'], ['a', 'b']]
    -- [ ['a', 'b'] ] -> ['a', 'b'] -> ['b'] -> 'b'
    let list3 = [['a', 'c', 'd'], ['a', 'b']]
    let element3 = head (tail (head (tail list3)))
    putStrLn $ "Element from list3: " ++ [element3]
    
    -- 4) Получить элемент 'b' из списка [['a', 'd'], ['b', 'c']]
    -- [ ['b', 'c'] ] -> ['b', 'c'] -> 'b'
    let list4 = [['a', 'd'], ['b', 'c']]
    let element4 = head (head (tail list4))
    putStrLn $ "Element from list4: " ++ [element4]
