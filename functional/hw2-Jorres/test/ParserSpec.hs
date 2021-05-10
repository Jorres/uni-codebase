module ParserSpec where

import           Parser

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck

mainParserSpec :: IO ()
mainParserSpec = hspec $ do
    describe "OK parser test" $ do
        it "OK on 'abc'" $ do
            runParser ok "abc" `shouldBe` Just((), "abc")
    describe "failAlways parser test" $ do
        it "Fail on 'abc'" $ do
            runParser failAlways "abc" `shouldBe` Nothing
    describe "stream parser test" $ do
        it "stream 'abc' 'abc' == Just ('abc', '')" $ do
            runParser (stream "abc") "abc" `shouldBe` Just ("abc", "")
        it "stream 'abc' 'ab' == Nothing" $ do
            runParser (stream "abc") "ab" `shouldBe` Nothing
    describe "Parentheses tests" $ do
        it "() == Just((), '')" $ do
            runParser psp "()" `shouldBe` Just((), "")
        it "(())() == Just((), '')" $ do
            runParser psp "(())()" `shouldBe` Just((), "")
    describe "parseInt tests" $ do
        it "+123" $ do
            runParser number "+123" `shouldBe` Just(123, "")
        it "-123" $ do
            runParser number "-123" `shouldBe` Just(-123, "")
        it "123" $ do
            runParser number "123" `shouldBe` Just(123, "")
    describe "listParser tests" $ do
        it "1 == [1]" $ do
            runParser (listParser 1) "1" `shouldBe` Just([1], "")
        it "1,2,3 == [1, 2, 3]" $ do
            runParser (listParser 3) "1,2,3" `shouldBe` Just([1,2,3], "")
        it "1 , 2 , 3 == [1, 2, 3]" $ do
            runParser (listParser 3) "1 , 2 , 3" `shouldBe` Just([1,2,3], "")
    describe "listlistParser tests" $ do
        it "1,1" $ do
            runParser listlistParser "1 , 1" `shouldBe` Just ([[1]], "")
        it "2, 1,+10  , 3,5,-7, 2" $ do
            runParser listlistParser "2, 1,+10  , 3,5,-7, 2" `shouldBe` Just ([ [1, 10], [5, -7, 2] ], "")
        it "Returns unparsed tail" $ do
            runParser listlistParser "2, 1,+10  , 3,5,-7, 2tail" `shouldBe` Just ([ [1, 10], [5, -7, 2] ], "tail")
