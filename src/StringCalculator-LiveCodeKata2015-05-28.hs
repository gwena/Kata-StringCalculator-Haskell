-- {-# OPTIONS_GHC -fno-warn-missing-signatures #-}
import Text.Parsec
import Control.Applicative

pp rule text = parse rule ("") text 

nat :: Parsec String () Integer
nat = read <$> many1 digit

singleDelimiter :: Parsec String () [String]
singleDelimiter = do
                    string "//"
                    c <- anyChar
                    newline
                    return [[c]]

multipleDelimiter :: Parsec String () [String]
multipleDelimiter = string "//" *> many1 sqrDelim <* newline
                    where sqrDelim = char '[' *> many1 (noneOf "]") <* char ']'

delimiter :: Parsec String () [String]
delimiter = choice [try multipleDelimiter, try singleDelimiter, return [",", "\n"]] 

extractNumber :: Parsec String () [Integer]
extractNumber = do
                  ds <- delimiter
                  sepBy nat (separator ds)
                   where separator = choice <$> map (try . string) 

add :: String -> Integer
add "" = 0
add xs = case (pp  extractNumber xs) of
           Right ns -> sum $ filter (<= 1000) ns
           Left _ -> -1

-- Testing Framework
assertEq :: Eq a => (a, a, String) -> String
assertEq (x, y, msg) = (if x == y then " . " else " *** FAIL *** ") ++ msg

runtest = mapM_ (putStrLn . assertEq)

tests = [(1, 1, "Testing Fwk - Pass"),
        (add "", 0, "should return 0 for empty string"),
        (add "1", 1, "should return value of one number"),
        (add "1,2", 3, "should return the sum of 2 numbers"),
        (add "1,2\n3", 6, "should cater for newline"),
        (add "//.\n1.2", 3, "should be able to specify single delimiter"),
        (add "1,2,3,1000,10001", 1006, "should remove number above 1000"),
        (add "1,-1,5", -1, "should failed for negative numbers"),
        (add "//[**][..]\n1**2..3", 6, "should be able to specifiy multiple delimiters")
        ]

main = runtest tests



