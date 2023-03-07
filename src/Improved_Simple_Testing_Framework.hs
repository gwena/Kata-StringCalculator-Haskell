import Text.Parsec
import Text.ParserCombinators.Parsec.Number
import Control.Applicative


-- Tests
tests = [
  ("Testing Framework - should pass for same Right", Right 1, Right 1),
  ("Testing Framework - should catch parsing error", parse nat "" "-2", parse nat "" "-2")
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
