module Database.Schema.Migrations.Filesystem.Parse
    ( migrationParser
    , parseDepsList
    , FieldName
    , Field
    , FieldSet
    )
where

import Data.Time.Clock ()
import Data.Maybe ( catMaybes )

import Text.ParserCombinators.Parsec

type FieldName = String
type Field = (FieldName, String)
type FieldSet = [Field]

-- |Parse a migration document and return a list of parsed fields and
-- a list of claimed dependencies.
migrationParser :: Parser [Field]
migrationParser = do
  result <- many (parseField <|> parseComment <|> parseEmptyLine)
  return $ catMaybes result

parseDepsList :: Parser [String]
parseDepsList =
    let parseMID = many1 (alphaNum <|> oneOf "-._")
    in do
      deps <- sepBy parseMID whitespace
      eol
      return deps

discard :: Parser a -> Parser ()
discard = (>> return ())

eol :: Parser ()
eol = (discard newline) <|> (discard eof)

whitespace :: Parser Char
whitespace = oneOf " \t"

requiredWhitespace :: Parser String
requiredWhitespace = many1 whitespace

parseFieldName :: Parser FieldName
parseFieldName = many1 (alphaNum <|> char '-')

parseComment :: Parser (Maybe Field)
parseComment = do
  discard $ do
    many whitespace
    char '#'
    manyTill anyChar eol
  return Nothing

parseEmptyLine :: Parser (Maybe Field)
parseEmptyLine = newline >> return Nothing

parseField :: Parser (Maybe Field)
parseField = do
  name <- parseFieldName
  char ':'
  many whitespace
  rest <- manyTill anyChar eol
  otherLines <- otherContentLines
  let value = rest ++ (concat otherLines)
  return $ Just (name, value)

otherContentLines :: Parser [String]
otherContentLines =
    many $ try $ (discard newline >> return "") <|> do
      ws <- requiredWhitespace
      rest <- manyTill anyChar eol
      -- Retain leading whitespace and trailing newline
      return $ ws ++ rest ++ "\n"
