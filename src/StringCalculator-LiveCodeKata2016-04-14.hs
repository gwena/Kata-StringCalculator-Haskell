import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Control.Applicative

singleDelimiter = do c <- string "//" *> anyChar <* newline
                     return [[c]]

multiDelimiter = string "//" *> many1 squareDelimiter <* newline
                   where squareDelimiter = char '[' *> many1 (noneOf "]") <* char ']'

delimiter = choice [try multiDelimiter, try singleDelimiter, return [",", "\n"]] 
    
extractNumbers = do ds <- delimiter
                    sepBy nat (separator ds) <* eof
                       where separator = choice <$> map (try . string)

add :: String -> MaybeInt
add str = sum <$> filter (<= 1000) <$>  ns 
          where ns = parse extractNumbers "Wrong Input Format" str 

-- Tests
tests = [
  ("should pass for same Right", Right 1, Right 1),
  ("should catch parsing error", parse nat "" "-2", parse nat "" "-2"),
  ("should return 0 for empty string", add "", Right 0),
  ("should return value of an integer", add "1", Right 1),
  ("should return the sum of integers separated by comma", add "1,2,3", Right 6),
  ("should return the sum of integers separated by comma or newline", add "1,2\n3", Right 6),
  ("should handle specification of single delimiter", add "//.\n1.2.3", Right 6),
  ("should ignore number greater than 1000", add "1,1000,10001,3", Right 1004),
  ("should ignore negatives number", add "-1", parse nat "" "-2"),
  ("should handle multiple delimiter", add "//[.][*]\n1.2*3", Right 6),
  ("should handle multiple delimiter", add "//[**][.]\n1**2.3", Right 6)
 ]

-- Super Simple Testing Framework & Main
type MaybeInt = Either ParseError Integer

assertEq :: (String, MaybeInt, MaybeInt) -> (String, Bool)
assertEq (test, Left _, Left _) = ("~" ++ test, True)
assertEq (test, actual, expected) 
   | actual == expected = (test, True)
   | otherwise = (test ++ indent "actual" actual ++ indent "expected" expected, False)

indent txt value = "\n\t" ++ txt ++ ": " ++ show value
 
formatResult :: (String, Bool) -> String
formatResult (test, b) = (if b then "." else redify "*** FAIL *** ") ++ test

redify = \str -> "\27[31m" ++ str ++ "\27[0m"

runtest =  mapM_ (putStrLn . formatResult . assertEq)

main = runtest tests
