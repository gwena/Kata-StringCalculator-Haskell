import Text.Parsec
import Control.Applicative

pp rule text = parse rule ("") text

nat :: Parsec String () Integer
nat = read <$> many1 digit

singleDelimiter :: Parsec String () [String]
singleDelimiter = do c <- string "//" *> anyChar <* newline
                     return [[c]]

multiDelimiter :: Parsec String () [String]
multiDelimiter = string "//" *> many1 squareDelim <* newline
                    where squareDelim = char '[' *> many1 (noneOf "]") <* char ']'

delimiter :: Parsec String () [String]
delimiter = choice [try multiDelimiter, try singleDelimiter, return [",","\n"]]

extractNumber :: Parsec String () [Integer]
extractNumber = do ds <- delimiter
                   sepBy nat (separator ds) <* eof
                        where separator = choice <$> map (try . string) 

add :: String -> Integer
add "" = 0
add st = case (pp extractNumber st) of
           Left _ -> -1
           Right ns -> sum $ filter (<= 1000) ns

-- Tests
tests = [(1,1,"Test Fwk - assert - should pass"),
         (add(""),0,"should return 0 for empty string"),
         (add("12"),12,"should return value of single number"),
         (add("1,2,3"),6,"should return sum of numbers separated by ,"),
         (add("1\n2,3"),6,"should return sum of numbers separated by , or newline"),
         (add("//.\n1.2.3"),6,"should be able to specify a single character as delimiter"),
         (add("//[**][---]\n20**30---40---1000---1001"),1090,"multiple delimiter of more than 1 character"),
         (add("1,\n"),-1,"two consecutives parameters should fail"),
         (add("//.3"),-1,"non terminated delimiter specifier should fail")
        ]

-- Testing Framework & Main
redify st = "\27[31m" ++ st ++ "\27[0m"

assertEq :: Eq a => (a,a,String) -> String
assertEq (x,y,msg) = (if x == y then " . " else redify(" *** FAIL *** ")) ++ msg

runtest = mapM_ (putStrLn . assertEq)

main = runtest tests
