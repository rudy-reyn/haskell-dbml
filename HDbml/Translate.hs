-- 10/20/22
-- Translate.hs
module HDbml.Translate where

type Sql = String
type Dbml = String

class Show a => ToSql a where
    toSql :: a -> Sql
    toSql = show

class Show a => ToDbml a where
    toDbml :: a -> Dbml
    toDbml = show
