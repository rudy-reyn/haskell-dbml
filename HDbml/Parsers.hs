-- 10/23/22
-- HDbml/Parsers.hs
module HDbml.Parsers where

import Data.Void (Void)
import Text.Megaparsec
import Text.Megaparsec.Char hiding (space, hspace)
import qualified Text.Megaparsec.Char as C
import qualified Text.Megaparsec.Char.Lexer as L

import HDbml.Definitions

-- Global parser definitions and generic parsers.
type Parser = Parsec Void String

lexeme = L.lexeme hspace
symbol = L.symbol hspace

hspace :: Parser ()
hspace = hidden C.hspace

space :: Parser ()
space = hidden C.space

paren = between (symbol "(") (symbol ")")
brackets = between (symbol "[") (symbol "]")

underscore = char '_'

comma = symbol ","
dot = symbol "."
hyphen = symbol "-"

collection btwn parser = btwn $ parser `sepBy` comma

list :: Parser a -> Parser [a]
list = collection brackets

tuple :: Parser a -> Parser [a]
tuple = collection paren

-- An identifer can contain any combination of ascii digits, letters, and underscores but can't start with a number.
ident :: Parser Ident
ident = label "identifier" $ do
    start <- letterChar <|> underscore
    end <- ident_chars
    return $ Ident (start:end)
    where ident_chars = many (letterChar <|> digitChar <|> underscore)

-- Numbers
integrals :: Parser String
integrals = integrals <|> (:) <$> char '-' <*> integrals
    where integrals = some (label "a number" numberChar)

fractionals :: Parser String
fractionals = (++) <$> integrals <*> ((:) <$> char '.' <*> integrals)

integer :: Parser Integer
integer = L.decimal

int :: Parser Int
int = L.decimal

double :: Parser Double
double = L.float

float :: Parser Float
float = L.float
