module Main where

import           Data.Char
import Control.Applicative

newtype Parser a =
  Parser
    { parse :: String -> Maybe (String, a)
    }

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parserA =
    Parser $ \input -> do
      (input', a) <- parse parserA input
      Just (input', f a)

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser $ \input -> Just (input, a)
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  parserA <*> parserB =
    Parser $ \input -> do
      (input', f) <- parse parserA input
      (input'', a) <- parse parserB input'
      Just (input'', f a)

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser (const Nothing)
  -- <|> :: Parser a -> Parser a -> Parser a
  parserA <|> parserB = Parser $ \input ->
    case parse parserA input of
      Just (input', a) -> Just (input', a)
      Nothing -> parse parserB input

instance Monad Parser where
  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  parserA >>= f = Parser $ \input -> do
     (input', a) <- parse parserA input
     parse (f a) input'

char :: Char -> Parser Char
char x = Parser f
  where
    f (y:ys)
      | y == x = Just (ys, y)
      | otherwise = Nothing
    f [] = Nothing

string :: String -> Parser String
string = traverse char

satisfies :: (Char -> Bool) -> Parser Char
satisfies f = Parser $ \input ->
  case input of
    (y:ys) | (f y) -> Just (ys, y)
    otherwise -> Nothing

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []
  
digit :: Parser Char
digit = satisfies isDigit

natural :: Parser Int
natural = read <$> some digit

integer :: Parser Int
integer = do char '-'
             n <- natural
             return (-n)
           <|> natural

normalChar :: Parser Char
normalChar = satisfies ((&&) <$> (/= '"') <*> (/= '\\'))

-- TODO: handle unicode escape (\u)
escapeChar :: Parser Char
escapeChar = ('"' <$ string "\\\"") <|>
             ('\\' <$ string "\\\\") <|>
             ('/' <$ string "\\/") <|>
             ('\b' <$ string "\\b") <|>
             ('\f' <$ string "\\f") <|>
             ('\n' <$ string "\\n") <|>
             ('\r' <$ string "\\r") <|>
             ('\t' <$ string "\\t")

stringLiteral :: Parser String
stringLiteral = char '"' *> many (normalChar <|> escapeChar) <* char '"'

-- Json

data JsonValue = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonInt Int
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  -- TODO: JsonFloat
  deriving (Show, Eq)

json :: Parser JsonValue
json = jsonNull <|> jsonBool <|> jsonInt <|> jsonString <|> jsonArray <|> jsonObject

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse <|> jsonString <|> jsonInt
  where
    jsonTrue = JsonBool True <$ string "true"
    jsonFalse = JsonBool False <$ string "false"

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonInt :: Parser JsonValue
jsonInt = JsonInt <$> integer

-- TODO: does not support lists of multiple types -> [123, "hello"]
-- TODO: handle whitespaces
jsonArray :: Parser JsonValue
jsonArray = fmap JsonArray $ char '[' *> arrayValues <* char ']'
  where arrayValues = sepBy (char ',') json

-- TODO: handle whitespaces
jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (char '{' *> assoc <* char '}')
 where assoc = sepBy (char ',') $ pair (stringLiteral <* char ':') json

pair :: Parser a -> Parser b -> Parser (a, b)
pair = liftA2 (,)

main :: IO ()
main = putStrLn "Hello, Haskell!"
