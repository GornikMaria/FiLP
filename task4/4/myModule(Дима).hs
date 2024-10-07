import qualified Data.Map.Internal as Map
import qualified Data.Set as Set
import qualified Data.Char as Char
import qualified Data.ByteString as Set
import qualified Data.Maybe as Maybe

-- Empty -- создает пустой Map
myEmpty :: Map.Map k a; -- создает пустую Map 
myEmpty = Map.Tip; -- обозначение пустого дерева

myEmptyTests :: IO();
myEmptyTests = do
    putStrLn ( 
        "myEmpty" ++
        "\tresult map size: " ++ show ( Map.size myEmpty ) ++ 
        "\texpected map size: 0" )


-- Insert -- вставляет новый элемент в Set
myInsert :: Ord a => a -> Set.Set a -> Set.Set a
myInsert item set = Set.fromList ( Set.toList set ++ [item] )

myInsertTests :: IO()
myInsertTests = do
    let testResult1 = myInsert 100 ( Set.fromList [ 1, 2, 3 ] )
    let testExpects1 = Set.insert 100 ( Set.fromList [ 1, 2, 3 ] )
    putStrLn (
        "myInsert 100 set([1, 2, 3])" ++
        "\tresult: " ++ show ( Set.toList testResult1 ) ++ 
        "\texpected: " ++ show ( Set.toList testExpects1 ) )

    let testResult2 = myInsert 100 ( Set.fromList [] )
    let testExpects2 = Set.insert 100 ( Set.fromList [] )
    putStrLn (
        "myInsert 100 set([])" ++
        "\t\tresult: " ++ show ( Set.toList testResult2 ) ++ 
        "\t\texpected: " ++ show ( Set.toList testExpects2 ) )


-- Take -- берет X элементов из списка
myTake :: Int -> [a] -> [a]
myTake count list 
    -- No items asked, return empty list
    | count < 1 = []
    | null list = []
    | otherwise = head list : myTake ( count - 1 ) ( tail list )  -- (head list - берет первый элемент)

myTakeTests :: IO()
myTakeTests = do
    -- Regular count and regular list
    putStrLn( 
        "myTake 1 [1,2,3]" ++
        "\tresult: " ++ show ( myTake 1 [1, 2, 3] ) ++ 
        "\texpected: " ++ show ( take 1 [1, 2, 3] ) )
    -- List is too small
    putStrLn( 
        "myTake 4 [1,2,3]" ++
        "\tresult: " ++ show ( myTake 4 [1, 2, 3] ) ++ 
        "\texpected: " ++ show ( take 4 [1, 2, 3] ) )
    -- Count is 0
    putStrLn( 
        "myTake 0 [1,2,3]" ++
        "\tresult: " ++ show ( myTake 0 [1, 2, 3] ) ++ 
        "\texpected: " ++ show ( take 0 [1, 2, 3] ) )
    -- Count is less than 0
    putStrLn( 
        "myTake -1 [1,2,3]" ++
        "\tresult: " ++ show ( myTake (-1) [1, 2, 3] ) ++ 
        "\texpected: " ++ show ( take (-1) [1, 2, 3] ) )
    -- List is empty
    putStrLn( 
        "myTake 1 []" ++
        "\t\tresult: " ++ show ( myTake 1 [] :: [Int] ) ++ 
        "\texpected: " ++ show ( take 1 [] :: [Int] ) )


-- IsSpace -- проверяет, является ли символ пустым символом, например, пробел 
myIsSpace :: Char -> Bool
myIsSpace symbol = Set.member symbol ( Set.fromList [' ', '\n', '\t'] ) --Set.member symbol: Проверяет, принадлежит ли символ множеству "пустых символов"

myIsSpaceTests :: IO()
myIsSpaceTests = do
    putStrLn(
        "myIsSpace 'a'" ++
        "\tresult: " ++ show ( myIsSpace 'a' ) ++
        "\texpected: " ++ show ( Char.isSpace 'a' ) )
    putStrLn(
        "myIsSpace '1'" ++
        "\tresult: " ++ show ( myIsSpace '1' ) ++
        "\texpected: " ++ show ( Char.isSpace '1' ) )
    putStrLn(
        "myIsSpace ' '" ++
        "\tresult: " ++ show ( myIsSpace ' ' ) ++
        "\texpected: " ++ show ( Char.isSpace ' ' ) )
    putStrLn(
        "myIsSpace '\\n'" ++
        "\tresult: " ++ show ( myIsSpace '\n' ) ++
        "\texpected: " ++ show ( Char.isSpace '\n' ) )
    putStrLn(
        "myIsSpace '\\t'" ++
        "\tresult: " ++ show ( myIsSpace '\t' ) ++
        "\texpected: " ++ show ( Char.isSpace '\t' ) )


-- IsNothing -- проверяет, является ли Maybe значение пустым 
myIsNothing :: Maybe a -> Bool
myIsNothing value = case value of
    Just _  -> False
    Nothing -> True

myIsNothingTests :: IO()
myIsNothingTests = do 
    putStrLn(
        "myIsNothing Nothing" ++
        "\tresult: " ++ show ( myIsNothing Nothing ) ++
        "\texpected: " ++ show ( Maybe.isNothing Nothing ) )
    putStrLn(
        "myIsNothing Just 1" ++
        "\tresult: " ++ show ( myIsNothing ( Just 1 ) ) ++
        "\texpected: " ++ show ( Maybe.isNothing ( Just 1 ) ) )

main :: IO()
main = do
    putStrLn "\tMy empty:"
    myEmptyTests
    
    putStrLn "\n\tMy insert:"
    myInsertTests
    
    putStrLn "\n\tMy take:"
    myTakeTests
    
    putStrLn "\n\tMy isSpace:"
    myIsSpaceTests

    putStrLn "\n\tMy isNothing:"
    myIsNothingTests