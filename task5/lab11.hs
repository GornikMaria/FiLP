-- 1. Вывести на экран сформированный список, данные для которого вводятся с клавиатуры: начальное значение, количество элементов, кратность. 
--    Учесть ситуацию, что начальное значение может не соответствовать указанной кратности.
--    Например- [14,21,28] 3 элемента списка, начинающиеся с 14, кратные 7

-- 1.1 Реализация
readInt :: IO Int
readInt = do
    input <- getLine
    return ( read input :: Int )

generateList :: Int -> Int -> Int -> [Int]
generateList initial elementsNumber multiplicity 
    | initial `mod` multiplicity == 0 = take elementsNumber [ initial, initial + multiplicity ..]
    | otherwise = do
        let initialK = initial `div` multiplicity + 1
        let list = [ multiplicity * ( initialK + 0 ), multiplicity * ( initialK + 1 ) ..];

        take elementsNumber ( initial : list )

-- 1.1 Тестирование
generateListTests :: IO()
generateListTests = do
    putStrLn(
        "generateList 10 3 10" ++
        "\tresult: " ++ show ( generateList 10 3 10) ++
        "\texpected: [10,20,30]" )
    putStrLn(
        "generateList 9 3 10" ++
        "\tresult: " ++ show ( generateList 9 3 10) ++
        "\texpected: [9,10,20]" )
    putStrLn(
        "generateList 11 3 10" ++
        "\tresult: " ++ show ( generateList 11 3 10) ++
        "\texpected: [11,20,30]" )
        
    -- Negative multiplicity
    putStrLn(
        "generateList 10 3 -2" ++
        "\tresult: " ++ show ( generateList 10 3 (-2) ) ++
        "\texpected: [10,8,6]" )
    -- This is because:
        -- k = 10 / -2 = -5
        -- 1st item = initial = 10
        -- 2nd item = multiliplicty * ( k + 1 ) = -2 * -4
        -- 3d item = multiliplicty * ( k + 2 ) = -2 * -3

    putStrLn(
        "generateList -10 3 10" ++
        "\tresult: " ++ show ( generateList (-10) 3 10 ) ++
        "\texpected: [-10 0 10]" )

    -- Negative count
    putStrLn(
        "generateList 10 -3 10" ++
        "\tresult: " ++ show ( generateList 10 (-3) 10 ) ++
        "\texpected: []" )

main :: IO ()
main = do
    generateListTests
    putStrLn "Enter the initial value:"
    initial <- readInt

    putStrLn "Enter the number of elements:"
    elementsNumber <- readInt

    putStrLn "Enter the multiplicity:"
    multiplicity <- readInt

    let resultList = generateList initial elementsNumber multiplicity
    putStrLn ( "Generated list: " ++ show resultList )