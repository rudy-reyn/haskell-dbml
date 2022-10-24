-- 10/18/22
-- Parser.hs
module HDbml.Dbml.Parser where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, hspace)
import qualified Text.Megaparsec.Char as C
import HDbml.Definitions
import HDbml.Parsers

pNote :: Parser String
pNote = do
    lexeme $ string' "Note:"
    note <- between (char q) (char q) $ takeWhileP Nothing (/= q)
    return note
    where q = '\''

pOptions :: Parser FieldOption
pOptions = choice
    [ PrimaryKey <$ (try (string' "pk") <|> string' "primary key")
    , Unique     <$ string' "unique"
    , Increment  <$ string' "increment"
    , Note      <$> try pNote
    , NotNull    <$ try (string' "not null")
    , Null       <$ try (string' "null")
    , OptionRef  <$> pRelation <*> pFieldRef ]

pRelation :: Parser Relationship
pRelation =
    ManyToMany <$ symbol "<>"
    <|> ManyToOne <$ symbol "<"
    <|> OneToMany <$ symbol ">"
    <|> OneToOne  <$ symbol "--"

pFieldRef :: Parser FieldRef
pFieldRef = do
    start <- ident
    dot
    end <- ident
    return $ FieldRef start end

pRef :: Parser (Ref FieldRef)
pRef = choice
    [ try $ parse (:<>) "<>"
    , parse ( :<)  "<"
    , parse ( :>)  ">"
    , parse (:--) "--" ]
  where
    parse relation sym = do
        left <- lexeme pFieldRef
        symbol sym
        right <- pFieldRef
        return $ left `relation` right

pType :: Parser Type
pType = choice
    [ Integer <$ try (string' "integer")
    , Int     <$ string' "int"
    , Double  <$ string' "double"
    , Float   <$ string' "float"
    , Bool    <$ try (string' "bool")
    , Blob    <$ string' "blob"
    , String  <$ string' "string"
    , Timestamp <$ string' "timestamp"

    , (VarChar . Just) <$> paren_int "varchar"
    , (Char . Just)    <$> paren_int "char"
    , VarChar Nothing   <$ string' "varchar"
    , Char Nothing      <$ string' "char" ]
    -- <|> Enum <$> string' "enum"
    where paren_int s = try $ lexeme (string' s) *> paren int

pField :: Parser Field
pField = do
    name <- lexeme ident
    typ <- lexeme pType
    options <- option [] $ list pOptions
    return (Field name typ options)

pTable :: Parser Table
pTable = do
    lexeme $ string "Table "
    name <- lexeme ident
    symbol "{"
    symbol "\n"
    space
    fields <- (try $ lexeme pField) `sepEndBy` symbol "\n"
    space
    note <- optional $ lexeme pNote
    space; symbol "}"
    return $ Table name fields note

-- TODO
pEnum :: Parser Enumeration
pEnum = do
    lexeme $ string "Enum "
    name <- lexeme ident
    symbol "{"
    symbol "\n"
    space
    enums <- lexeme ident `sepEndBy` symbol "\n"
    space; symbol "}"
    return $ Enum name enums
