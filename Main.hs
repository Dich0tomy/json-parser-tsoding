-- Needed for case lambda to work; see line 82.
{-# LANGUAGE LambdaCase #-}

-- To be able to implement and use Alternative.
import Control.Applicative
-- To be able to use isDigit and isSpace.
import GHC.Unicode
-- To use (second)
import Data.Bifunctor

{--
Jsonvalue is a new data type that has several constructors indicating it's possible values
Each constructor can be parametrized by a different thing
--}
data JsonValue =
 JsonNull
 | JsonBool Bool
 | JsonNumber Integer
 | JsonString String
 | JsonArray [JsonValue]
 | JsonObject [(String, JsonValue)]
 deriving(Eq, Show)

{--
Parser is a new record type that has one named field - `runParser`.
Haskell automagically generates functions with the same names as records fields
So the type of `runParser` is `runParser :: Parser a -> String -> Maybe (String, a)`
Then we can do `runParser someArbitraryParser`, say `runParser Parser JsonValue`,
and the resulting type will be `Maybe (String, JsonValue)`.
--}
newtype Parser a = Parser {
  runParser :: String -> Maybe (String, a)
}

{--
We need to implement an instance of a Functor for Parser.
Basically kinda like implementing an interface.

We need it in order to implement an instance of Applicative.

The bare minimum for a Functor is to implement `fmap`, so we do just that.
The do notation is still a little bit of a weirdness to me (quite imperative), but it.. works.
Without I'd have to write some case expressions and shits I suppose.
--}
{-- 
`second` is a bifunctor function with the following type `(b -> c) -> p a b -> p a c`
--}
instance Functor Parser where
  fmap f (Parser p) = Parser (fmap (second f) . p)

{-- This is the original code --}
-- instance Functor Parser where
--   fmap f (Parser p) = Parser $
--     \input -> do
--       (rest, a) <- p input
--       Just (rest, f a)

{--
Here we say that Parser is an Applicative and the operators on it look like follows.

The minimum is `pure a :: f a` and `(<*>) f (a -> b) -> f a -> f b` so we implement just that.
--}
instance Applicative Parser where
  pure a = Parser $ \input -> Just (input, a)
  (<*>) (Parser p1) (Parser p2) = Parser $ \input -> do
    (rest, f) <- p1 input
    (rest', a) <- p2 rest
    Just (rest', f a)

{--
`Alternative` is an operation that has a notion of an empty Alternative and the alternative operator.

The alternative operator returns whichever side is not empty.

That is - for our parser an `empty` parser is one, that's failed - `Parser $ \_ -> Nothing`
And the alternative operator makes use of the fact that the stored maybe is also an alternative.
--}
instance Alternative Parser where
  empty = Parser $ const Nothing
  (<|>) (Parser p1) (Parser p2) = Parser $ \input -> p1 input <|>  p2 input

{--
charPaser takes a character and returns a parser that parses it.
charParser 'x' returns
  \str -> case str of
    y:ys | y == 'x' -> Just (ys, c)
    _ -> Nothing
Which is of type (String -> Maybe (String, Char))

The pattern matching checks
  1.1. If the string has at least one character (y:ys)
  1.2. If the first character matches the character we want to parse (y == c)
    Then it returns `Just (ys, c)`

  2.1. If the input is empty of the first character is not the same
    Then it returns Nothing
--}

{-- This code requires an extension called lamba-case --}
charParser :: Char -> Parser Char
charParser c = Parser $ \case
    y:ys | y == c -> Just (ys, c)
    _ -> Nothing

{-- This is the original code --}
-- charParser :: Char -> Parser Char
-- charParser c = Parser $ \str -> case str of
--   y:ys | y == c -> Just (ys, c)
--   _ -> Nothing

{-- Apparently `traverse X` is the same as `map . sequenceA X`--}
stringParser :: String -> Parser String
stringParser = traverse charParser

{--
<$ is a Functor thing and it's basically like `(\_ -> JsonNull) <$> stringParser "null"`
So instead of having a useless lambda we can just provide the value.

btw.`const JsonNull` is the same as `(\_ -> JsonNull)`.
--}
jsonNull :: Parser JsonValue
jsonNull = JsonNull <$ stringParser "null"

{--
Here we actually have to use the full fmap version (fmap == <$>) because we have to write a function
that does pattern matching on the parsed string.
--}
jsonBool :: Parser JsonValue
jsonBool = f <$> (stringParser "true" <|> stringParser "false") where
  f "true" = JsonBool True
  f "false" = JsonBool False

{--
At this point I totally didn't understand what the spanParser is for.
--}
spanParser ::(Char -> Bool) -> Parser String
spanParser pred = Parser $ \input ->
  let (token, rest) = span pred input
  in Just (rest, token)

notNull :: Parser [a] -> Parser [a]
notNull (Parser p) = Parser $ \input -> do
  (rest, result) <- p input
  if null result
    then Nothing
    else Just (rest, result)

jsonNumber :: Parser JsonValue
jsonNumber = f <$> notNull (spanParser isDigit) where
  f ds = JsonNumber $ read ds

stringLiteral :: Parser String
stringLiteral = charParser '"' *> spanParser (/= '"') <* charParser '"'

jsonString :: Parser JsonValue
jsonString = JsonString <$> stringLiteral

ws :: Parser String
ws = spanParser isSpace

{--
Now this is some fine magic here.
<$> -> fmap
<*> -> weird thing from Applicative.
Applies a function wrapped in Applicative to a list of things wrapped in an Applicative.

Now if that fails we just return Parser of an empty list.
--}
sepBy :: Parser a -> Parser b -> Parser [b]
sepBy sep elem = (:) <$> elem <*> many (sep *> elem) <|> pure []

jsonArray :: Parser JsonValue
jsonArray = JsonArray <$> (charParser '[' *> ws *> elements <* ws <* charParser ']') where
  elements = comma `sepBy` jsonValue
  comma = ws *> charParser ',' <* ws

jsonObject :: Parser JsonValue
jsonObject = JsonObject <$> (charParser '{' *> ws *> sepBy (ws *> charParser ',' <* ws) pair <* ws <* charParser '}') where
  pair = (\k _ v -> (k, v)) <$> stringLiteral <*> (ws *> charParser ':' <* ws) <*> jsonValue

{--
The general `jsonValue` parser is just a combination of all the other parsers.
--}
jsonValue :: Parser JsonValue
jsonValue = jsonNull <|> jsonBool <|> jsonNumber <|> jsonString <|> jsonArray <|> jsonObject

main :: IO ()
main = undefined
