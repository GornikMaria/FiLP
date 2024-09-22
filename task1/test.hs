-- Импортируем стандартный модуль Prelude
import Prelude

main :: IO ()
main = do
    -- Определяем кортеж
    let myTuple = ((1, 'a'), "abc")
    
    -- Извлечем первый элемент кортежа, который является другим кортежем
    let innerTuple = fst myTuple
    
    -- Извлечем второй элемент из внутреннего кортежа, который является символом
    let charValue = snd innerTuple
    
    -- Выведем значение на экран
    putStrLn $ "The character extracted is: " ++ [charValue]
