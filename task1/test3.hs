-- Способ 1: Использование take
main1 :: IO ()
main1 = do
-- [1, 3 ..] создаёт бесконечный список нечётных чисел, начиная с 1 и увеличивая на 2
-- take 20 выбирает первые 20 элементов из этого списка
    let oddNumbers = take 20 [1, 3 ..]
    print oddNumbers

-- Способ 2: Использование list comprehension
main2 :: IO ()
main2 = do
-- Создаем список из первых 20 нечётных чисел
    let oddNumbers = [x | x <- [1..], odd x]
    let first20OddNumbers = take 20 oddNumbers
    print first20OddNumbers

-- Способ 3: Использование map
main3 :: IO ()
main3 = do
    let oddNumbers = map (\x -> 2*x + 1) [0..19]
    print oddNumbers

main :: IO ()
main = do
    putStrLn "Способ 1:"
    main1
    putStrLn "Способ 2:"
    main2
    putStrLn "Способ 3:"
    main3
