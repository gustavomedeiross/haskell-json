module Parser where

import           Control.Applicative
import           Data.Char
import           GHC.Float           (fabsDouble, int2Float)

newtype Parser a =
  Parser
    { parse :: String -> Either ParserError (String, a)
    }

newtype ParserError =
  ParserError
    { message :: String
    }
  deriving (Show, Eq)

instance Functor Parser where
  -- fmap :: (a -> b) -> Parser a -> Parser b
  fmap f parserA =
    Parser $ \input -> do
      (input', a) <- parse parserA input
      Right (input', f a)

instance Applicative Parser where
  -- pure :: a -> Parser a
  pure a = Parser $ \input -> Right (input, a)
  -- <*> :: Parser (a -> b) -> Parser a -> Parser b
  parserA <*> parserB =
    Parser $ \input -> do
      (input', f) <- parse parserA input
      (input'', a) <- parse parserB input'
      Right (input'', f a)

instance Alternative Parser where
  -- empty :: Parser a
  empty = Parser $ const $ Left ParserError {message = "Empty"}
  -- <|> :: Parser a -> Parser a -> Parser a
  parserA <|> parserB =
    Parser $ \input ->
      case parse parserA input of
        Right (input', a) -> Right (input', a)
        Left _            -> parse parserB input

instance Monad Parser where
  -- >>= :: Parser a -> (a -> Parser b) -> Parser b
  parserA >>= f =
    Parser $ \input -> do
      (input', a) <- parse parserA input
      parse (f a) input'

char :: Char -> Parser Char
char x = parseIf ("'" ++ [x] ++ "'") (x ==)

string :: String -> Parser String
string = traverse char

parseIf :: String -> (Char -> Bool) -> Parser Char
parseIf desc f = Parser fn
  where
    fn (y:ys)
      | f y = Right (ys, y)
      | otherwise =
        Left
          ParserError
            {message = "Expected " ++ desc ++ ", found '" ++ [y] ++ "'"}
    fn [] =
      Left ParserError {message = "Unexpected end of file, expected " ++ desc}

sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep element = (:) <$> element <*> many (sep *> element) <|> pure []

digit :: Parser Char
digit = parseIf "digit" isDigit

natural :: Parser Int
natural = read <$> some digit

integer :: Parser Int
integer = natural <|> char '-' *> (negate <$> natural)

-- TODO: fix rounding problems
float :: Parser Float
float = do
  isPositive <- False <$ char '-' <|> pure True
  integral <- natural
  char '.'
  decimal <- natural
  return (floatFromParts isPositive integral decimal)

floatFromParts :: Bool -> Int -> Int -> Float
floatFromParts isPositive integral decimal = signal * (int + dec)
  where
    int = int2Float integral
    dec = shiftToDecimal $ int2Float decimal
    signal =
      if isPositive
        then 1
        else -1

-- 1592.52 -> 0.159252
shiftToDecimal :: Float -> Float
shiftToDecimal x
  | x >= 1 = shiftToDecimal $ x / 10
  | otherwise = x

whitespace :: Parser ()
whitespace = () <$ parseIf "whitespace" (== ' ')

ws :: Parser ()
ws = () <$ many whitespace

normalChar :: Parser Char
normalChar = parseIf "non-escape char" ((&&) <$> (/= '"') <*> (/= '\\'))

-- TODO: handle unicode escape (\u)
escapeChar :: Parser Char
escapeChar =
  ('"' <$ string "\\\"") <|> ('\\' <$ string "\\\\") <|> ('/' <$ string "\\/") <|>
  ('\b' <$ string "\\b") <|>
  ('\f' <$ string "\\f") <|>
  ('\n' <$ string "\\n") <|>
  ('\r' <$ string "\\r") <|>
  ('\t' <$ string "\\t")

stringLiteral :: Parser String
stringLiteral = char '"' *> many (normalChar <|> escapeChar) <* char '"'

pair :: Parser a -> Parser b -> Parser (a, b)
pair = liftA2 (,)

-- Json
data JsonValue
  = JsonNull
  | JsonBool Bool
  | JsonString String
  | JsonInt Int
  | JsonFloat Float
  | JsonArray [JsonValue]
  | JsonObject [(String, JsonValue)]
  deriving (Show, Eq)

json :: Parser JsonValue
json =
  jsonNull <|> jsonBool <|> jsonFloat <|> jsonInt <|> jsonString <|> jsonArray <|>
  jsonObject

jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ string "null"

jsonBool :: Parser JsonValue
jsonBool = jsonTrue <|> jsonFalse
  where
    jsonTrue = JsonBool True <$ string "true"
    jsonFalse = JsonBool False <$ string "false"

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

jsonInt :: Parser JsonValue
jsonInt = JsonInt <$> integer

jsonFloat :: Parser JsonValue
jsonFloat = JsonFloat <$> float

jsonArray :: Parser JsonValue
jsonArray = fmap JsonArray $ char '[' *> ws *> arrayValues <* ws <* char ']'
  where
    arrayValues = sepBy comma json
    comma = ws *> char ',' <* ws

jsonObject :: Parser JsonValue
jsonObject = fmap JsonObject $ char '{' *> ws *> assoc <* ws <* char '}'
  where
    assoc = sepBy comma $ pair (stringLiteral <* ws <* char ':' <* ws) json
    comma = ws *> char ',' <* ws
