{-# LANGUAGE LambdaCase #-}
-- 10/18/22
-- Translate.hs
module HDbml.Dbml.Translate where

import Data.Char (toLower)
import Data.List (unlines, intercalate)

import HDbml.Definitions
import HDbml.Translate

instance ToDbml Ident where
    toDbml (Ident ident) = ident

instance ToDbml FieldRef where
    toDbml (FieldRef schema field) = toDbml schema ++ "." ++ toDbml field

instance ToDbml a => ToDbml (Ref a) where
    toDbml ref = "Ref: " ++
      case ref of
        (x :< y)  -> toDbml x ++ " < "  ++ toDbml y   -- many-to-one
        (x :> y)  -> toDbml x ++ " > "  ++ toDbml y   -- one-to-many
        (x :-- y) -> toDbml x ++ " -- " ++ toDbml y   -- one-to-one
        (x :<> y) -> toDbml x ++ " <> " ++ toDbml y   -- many-to-many

instance ToDbml FieldOption where
    toDbml options =
      case options of
        PrimaryKey -> "primary key"
        Null -> "null"
        NotNull -> "not null"
        Unique -> "unique"
        Increment -> "Increment"
        Default value -> "default: " ++ value
        Note note -> "note: '" ++ note ++ "'"

instance ToDbml Type where
    toDbml = \case
        VarChar Nothing -> "varchar"
        VarChar (Just len) -> "varchar(" ++ show len ++ ")"
        Char Nothing -> "char"
        Char (Just len) -> "char(" ++ (show len) ++ ")"
        Custom custom -> custom
        typ -> map toLower (show typ)

instance ToDbml Field where
    toDbml (Field name typ options) = toDbml name ++ " "
        ++ toDbml typ ++ " "
        ++ case options of
             [] -> ""
             opts -> show (map toDbml options)

instance ToDbml Table where
    toDbml (Table name fields note) = "Table "
        ++ toDbml name ++ " {\n"
        ++ unlines ["\t" ++ toDbml field | field <- fields]
        ++ case note of
            Just (note') -> "\nNote: '" ++ note' ++ "'\n}"
            Nothing -> "}"

instance ToDbml Enumeration where
    toDbml (Enum name enums) =
        "Enum "
        ++ toDbml name ++ " "
        ++ case enums of
            [] -> []
            _  -> ("{\n"
                ++ unlines ["\t" ++ toDbml e | e <- enums]
                ++ "}")

