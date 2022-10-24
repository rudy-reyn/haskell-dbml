{-# LANGUAGE OverloadedStrings #-}
-- 10/20/22
-- HDbml.hs
module HDbml
    ( module HDbml.Definitions
    , module HDbml.Translate
    , module HDbml.Dbml
    , module HDbml.Sql
) where

import HDbml.Definitions
import HDbml.Translate
import HDbml.Sql
import HDbml.Dbml
import HDbml.Parsers

import Text.Megaparsec

main = do
    let refrence = FieldRef "users" "id" :> FieldRef "contact" "user_id"
        puts = putStrLn
        putsE = putStrLn ""

    print refrence
    puts $ toDbml refrence
    puts $ toSql Int

    parseTest pFieldRef "customers.id"
    parseTest ident "customers"

    parseTest pType "integer"
    parseTest pType "varChar ( 255)"
    parseTest pType "varChar"
    putsE
    let field = "id Int [note: 'primary key', pk, not null]"

    puts $ "Field test:\n" ++ field
    parseTest pField field
    puts $ "toSql:\n" ++ field
    print $ fmap toSql (runParser pField "" field)
    putsE

    puts $ "Field test:\nid Int"
    parseTest pField "id Int"
    putsE

    puts "customers.id <> purchases.customerId"
    parseTest pRef "customers.id <> purchases.customerId"
    putsE

    let tableStr =  "Table customers {\n id integer [pk, > purchases.customerId, <> contact.customerId]\n firstName string\n lastName string []\n note: 'test'\n}"
    puts tableStr
    parseTest pTable tableStr
    puts $ case parse pTable "" tableStr of
                Right res -> toSql res
                Left _ -> ""

    puts "Enum test:"
    let enumStr = "Enum RGB {\n\tred\n\tgreen\n\tblue\n}"
        enumDbml = Enum "RGB" ["red", "green", "blue"]

    puts $ "Text:\n" ++ enumStr
    putStr "Enum: "
    print enumDbml
    puts $ "Parsed: "
            ++ case parse pEnum "" enumStr of
                Right res -> show res
                Left _ -> ""

    puts $ "toDbml: "
    puts $ toDbml enumDbml
