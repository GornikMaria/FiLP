-- Определим функцию для вычисления пирамидального числа Ферма
pyramidNumber :: Integer -> Integer
pyramidNumber k = k * (k + 1) * (k + 2) `div` 6

-- Определим список первых 50 пирамидальных чисел Ферма
fermatPyramidNumbers :: [Integer]
fermatPyramidNumbers = take 50 [pyramidNumber k | k <- [1..]]

main :: IO ()
main = do
    print fermatPyramidNumbers
