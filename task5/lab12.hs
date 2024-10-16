import System.Environment (getArgs)
import Data.Char (isPunctuation)
import System.IO

main = do
    args <- getArgs
    case args of
        [sourceFile, targetFile] -> do
            putStrLn "Введите символ для замены знаков пунктуации:"
            replacementChar <- getChar
            content <- readFile sourceFile
            let newContent = map (\c -> if isPunctuation c then replacementChar else c) content
            writeFile targetFile newContent
            putStrLn $ "Файл '" ++ sourceFile ++ "' был скопирован в '" ++ targetFile ++ "' с заменой знаков пунктуации на '" ++ [replacementChar] ++ "'."
        _ -> putStrLn "Ошибка: Необходимо указать два аргумента - исходный файл и целевой файл."