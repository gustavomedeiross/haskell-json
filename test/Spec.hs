module Main where

import           Parser
import           Test.Tasty
import           Test.Tasty.HUnit

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parserTests, jsonParserTests]

parserTests =
  testGroup
    "Parser Tests"
    [ testCase "Can parse a char" $ parse (char 'a') "abc" @?= Right ("bc", 'a')
    , testCase "Parsing a char can fail" $
      parse (char 'a') "bc" @?=
      Left ParserError {message = "Expected 'a', found 'b'"}
    , testCase "Can parse a string" $
      parse (string "hello") "hello world" @?= Right (" world", "hello")
    , testCase "Can parse a positive integer" $
      parse integer "123" @?= Right ("", 123)
    , testCase "Can parse a negative integer" $
      parse integer "-123" @?= Right ("", -123)
    , testCase "Can parse a float" $ parse float "123.23" @?= Right ("", 123.23)
    , testCase "Can parse a negative float" $
      parse float "-1592.234" @?= Right ("", -1592.234)
    ]

jsonParserTests =
  testGroup
    "Json Parser Tests"
    [ testCase "Can parse null" $ parse json "null" @?= Right ("", JsonNull)
    , testCase "Can parse the boolean true" $
      parse json "true" @?= Right ("", JsonBool True)
    , testCase "Can parse the boolean false" $
      parse json "false" @?= Right ("", JsonBool False)
    , testCase "Can parse a string literal" $
      parse json "\"a string literal\"" @?=
      Right ("", JsonString "a string literal")
    , testCase "Can parse a json int" $
      parse json "1234" @?= Right ("", JsonInt 1234)
    , testCase "Can parse a json float" $
      parse json "12.34" @?= Right ("", JsonFloat 12.34)
    , testCase "Can parse a json array" $
      parse json "[1, true, \"hello\"]" @?=
      Right ("", JsonArray [JsonInt 1, JsonBool True, JsonString "hello"])
    , testCase "Can parse a json object" $
      parse json "{ \"hello\": \"world\", \"something\": 123 }" @?=
      Right
        ( ""
        , JsonObject [("hello", JsonString "world"), ("something", JsonInt 123)])
    , testCase "Can parse a json object with an array" $
      parse json "{ \"array\": [1, 2] }" @?=
      Right ("", JsonObject [("array", JsonArray [JsonInt 1, JsonInt 2])])
-- TODO: fix
--     , testCase "Parsing an array with missing ] should fail" $
--       parse json "[true, true}" @?= Left ParserError {message = "Expected ']', found '}'"}
    ]
