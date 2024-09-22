-- Определим функцию для вычисления треугольного числа
triangleNumber :: Integer -> Integer
triangleNumber n = n * (n + 1) `div` 2

-- Определим список первых 50 треугольных чисел Ферма
fermatTriangleNumbers :: [Integer]
fermatTriangleNumbers = [triangleNumber n | n <- [1..50]]

main :: IO ()
main = do
    print fermatTriangleNumbers