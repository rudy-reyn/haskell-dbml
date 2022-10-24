-- 10/18/22
-- HDbml/Definitions.hs
module HDbml.Definitions where

import Data.String (IsString (..))

data Table
    = Table
        { schemaName :: Ident
        , schemaFields :: [Field]
        , schemaNote :: Maybe String }
    deriving (Eq, Show)

data Field
    = Field
        { fieldName :: Ident
        , fieldType :: Type
        , fieldOptions :: [FieldOption] }
    deriving (Eq, Show)

newtype Ident = Ident String
    deriving (Eq, Show)

-- Used for OverloadedStrings
instance IsString Ident where
    fromString = Ident

data Type
    = Int
    | Integer
    | Double
    | Float
    | Bool
    | Blob
    | String
    | VarChar (Maybe Int)
    | Char (Maybe Int)
    | Timestamp
    | Custom String
    deriving (Eq, Show)

data Enumeration = Enum
    -- Used for categorical types
    { enumName :: Ident
    , enumValues :: [Ident] }
    deriving (Eq, Show)

data FieldOption
    = PrimaryKey
    | Null
    | NotNull
    | Unique
    | Default String
    | Increment
    | OptionRef Relationship FieldRef
    | Note String
    deriving (Eq, Show)

data FieldRef
    = FieldRef
        { schemaId :: Ident
        , fieldId :: Ident }
    deriving (Eq, Show)

data Relationship
    = ManyToOne
    | OneToMany
    | OneToOne
    | ManyToMany
    deriving (Eq, Show)

data Ref a
    = a :< a      -- many-to-one
    | a :> a      -- one-to-many
    | a :-- a     -- one-to-one
    | a :<> a     -- many-to-many
    deriving (Eq, Show)
