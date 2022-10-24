{-# LANGUAGE LambdaCase #-}
-- 10/19/22
-- Sql/Translate.hs
module HDbml.Sql.Translate where

import Data.Char (isLetter, toUpper, isDigit)
import Data.List (partition, intercalate)
import Control.Exception (throw)

import HDbml.Sql.Errors
import HDbml.Definitions
import HDbml.Translate
import HDbml.Utils

instance ToSql Ident where
    -- Validation needs to be done at both the parsing and translation stage
    -- because the table api is agnostic of the parser, but this could be wrapped
    -- in a monad. This isn't terribly inefficient at the moment but could be
    -- as this scales and with large files.
    toSql (Ident []) = throw $ NameError "empty identifer"
    toSql (Ident name@(x:xs))
         | validChar x = x : go xs
         | otherwise = throw invalidName
        where
            go [] = []
            go (c:cs)
                | validChar c || isDigit c = c : go cs
                | otherwise = throw invalidName
            validChar char = isLetter char || char == '_'
            invalidName = NameError $ take 10 name ++ "... is an invalid identifer"

instance ToSql Table where
    toSql (Table tableName fields note) =
      case note of
        Nothing  -> "\n"
        Just com -> "-- " ++ com ++ "\n"
      ++ "CREATE TABLE "
      ++ toSql tableName ++ " ("
      ++ fields_to_sql fields
      ++ "\n)"
      where
        dup_field_msg name =
            "Table " ++ toSql tableName ++ " contains duplicate fields" ++ toSql name

        -- Throws an error if duplicate fields exist in a table.
        validate_fields = go []
          where
            go _ [] = []
            go visited (fld@(Field name _ _):xs)
              | name `elem` visited = throw (InvalidTable $ dup_field_msg name)
              | otherwise = fld : go (name:visited) xs

        is_opt_ref (OptionRef _ _) = True
        is_opt_ref _ = False

        -- Foreign keys are represented separately
        configure_field (Field name typ opts) =
            let (refs, updated) = partition is_opt_ref opts
             in ((name, map toSql refs), toSql $ Field name typ updated)

        configure_fk (name, []) = []
        configure_fk (name, refs) =
            "FOREIGN KEY "
            ++ toSql name
            ++ " REFERENCES ("
            ++ intercalate ", " refs ++ ")"

        isEmpty [] = True
        isEmpty _ = False

        fields_to_sql flds = do
          let configure = map configure_fk . filter (not . isEmpty . snd)
          case unzip $ map configure_field (validate_fields flds) of
            ([], []) -> ""
            (refs, flds) -> "\n\t"
                ++ intercalate ",\n\t" flds
                ++ case configure refs of
                    [] -> ""
                    foreign_keys -> ",\n\t" ++ intercalate ",\n\t" foreign_keys

instance ToSql Field where
    toSql (Field name typ opts) = intercalate " " $
        [toSql name, toSql typ] ++ map toSql (go Nothing [] opts)
      where
        -- Removes duplicate constraints,
        -- gets the default value while ensuring multiple defaults are not passed
        -- and configures notes as a singular comment and appends to end of the list.
        go :: Maybe FieldOption -- Default Value
           -> String        -- Comment
           -> [FieldOption] -- Constraints to configure
           -> [FieldOption] -- Configured Constraints
        go default_ comment options =
          case (default_, comment, options) of
            (Nothing,   [], [])           -> []
            (Nothing,  com, [])           -> [Note com]
            (Just def, com, [])           -> [def, Note com]
            (def,       [], Note x:xs)    -> go def x xs
            (def,      com, Note x:xs)    -> go def (com ++ "; " ++ x) xs
            (Nothing,  com, Default x:xs) -> go (Just $ Default x) com xs
            (Just _,     _, Default _:_)  -> throw $ multiple_defaults_err
            -- Only adds constraint if it doesn't already exist
            (def,      com, x:xs)         -> x ?: go def com xs
        multiple_defaults_err =
            InvalidConstraints "Can't have more than one default field value"

instance ToSql FieldRef where
    -- This is dialect dependent, some sql dialects use table(field) syntax instead.
    toSql (FieldRef schema field) = toSql schema ++ "." ++ toSql field

instance ToSql FieldOption where
    toSql = \case
        PrimaryKey -> "PRIMARY KEY"
        Null -> "NULL"
        NotNull -> "NOT NULL"
        Unique -> "UNIQUE"
        Increment -> "INCREMENT"
        Default value -> "DEFAULT " ++ value
        OptionRef _ ref -> toSql ref
        Note note -> "-- " ++ note

instance ToSql Type where
    toSql = \case
        VarChar Nothing -> "VARCHAR(255)"
        VarChar (Just len) -> "VARCHAR(" ++ show len ++ ")"
        Char Nothing -> "CHAR(255)"
        Char (Just len) -> "CHAR(" ++ show len ++ ")"
        Custom custom -> custom
        typ -> map toUpper $ show typ
