import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Control.Applicative

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

add :: String -> Either ParseError Integer
add st = sum <$> (filter (<= 1000) <$> ns)
           where ns = parse extractNumber ("Wrong String Calculator format") st 

-- Tests
tests = [
  (Right 1, Right 1, "Testing Functions - should pass for same Right"),
  (add "", Right 0, "should return 0 for empty string"),
  (add "", Right 1, "should fail - testing Redify as well - should be Red"),
  (add "12", Right 12, "should return value of single number"),
  (add "1,2,3", Right 6, "should return sum of numbers separated by ,"),
  (add "1\n2,3", Right 6, "should return sum of numbers separated by , or newline"),
  (add "//.\n1.2.3", Right 6, "should be able to specify a single character as delimiter"),
  (add "1,1000,1001", Right 1001, "should ignore number bigger than 1000"),
  (add "//[*][-]\n20*30-40",Right 90, "should cater for multiple delimiters"),
  (add "//[**][---]\n20**30---40", Right 90, "should cater for multiple long delimiters"),
  (add "//[***][**]\n1***2**3***4", Right 10, "should cater for delimiter subset of previous ones")
  -- Negative Tests - Commented as need Left ParseError
  -- (add "1,\n", Left "", "should fail for 2 consecutives separators, see Ray Osherove's 3.2 "),
  -- (add "//.\n5.5xxx.3", Left "", "should fail for rubbish between number and separators"),
  -- (add "1,2,3,4,5,6%%^&^*&^*&", Left "", "should fail for stream with rubbish at the end"),
  -- (add "-1", Left "", "should fail for one negative number"),
  -- (add "1,2,-30,4,-20,100", Left "", "should fail for negative numbers between natural ones")
 ]

-- Testing Functions & Main
assertEq :: (Either ParseError Integer, Either ParseError Integer, String) -> (Bool, String)
assertEq (actual, expected, testMsg) = (actual == expected, testMsg) 

formatResult :: (Bool, String) -> String
formatResult (b, msg) = (if b then " . " else redify " *** FAIL *** ") ++ msg

redify st = "\27[31m" ++ st ++ "\27[0m"

runtest =  mapM_ (putStrLn . formatResult . assertEq)

main = runtest tests
