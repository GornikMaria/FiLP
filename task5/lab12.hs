-- 2. Скопировать информацию из одного файла в другой, заменив знаки  пунктуации заданным с клавиатуры символом. Имена файлов указываются в командной строке.

-- 2.1 Реализация
import System.IO
import Data.Char (isPunctuation)

replacePunctuation :: Char -> String -> String
replacePunctuation replacement = map (\c -> if isPunctuation c then replacement else c)

readString :: String -> IO String
readString description = do
    putStrLn description
    input <- getLine
    return input

readChar :: String -> IO Char
readChar description = do
    input <- readString description
    return (head input)

main :: IO ()
main = do
  inputFile <- readString "Введите входной файл:"
  outputFile <- readString "Введите выходной файл:"
  replacementChar <- readChar "Введите символ, которым заменить:"

  fileContent <- readFile inputFile

  let outputFileContent = replacePunctuation replacementChar fileContent

  writeFile outputFile outputFileContent