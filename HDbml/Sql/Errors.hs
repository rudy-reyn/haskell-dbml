-- 10/19/22
-- Sql/Errors.hs
module HDbml.Sql.Errors where

import Data.Typeable (Typeable (..))
import Control.Exception (Exception, throw)

import HDbml.Definitions

data SqlError
    = FieldError String
    | NameError String
    | InvalidField String
    | InvalidConstraints String
    | InvalidTable String
    deriving (Eq, Show)

instance Exception SqlError
