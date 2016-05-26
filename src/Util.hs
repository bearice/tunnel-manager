{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE FlexibleContexts #-}
module Util where

import Web.Scotty.Trans
import Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Lazy as L
import Data.Aeson hiding (json)
import Data.Aeson.Types
import Data.Char
import GHC.Generics

mkJson :: (ScottyError e, Monad m) => [Pair] -> ActionT e m ()
mkJson = json . object

mkError :: (ScottyError e, Monad m) => Value -> ActionT e m ()
mkError err = mkJson [("Err",err)]

mkEmptyObj :: (ScottyError e, Monad m) => ActionT e m ()
mkEmptyObj = json emptyObject

upperFirstChar :: String -> String
upperFirstChar [] = []
upperFirstChar (h:t) = toUpper h:t

lowerFirstChar :: String -> String
lowerFirstChar [] = []
lowerFirstChar (h:t) = toLower h:t

toCamalJSON :: (Generic a, GToJSON (Rep a)) => a -> Value
toCamalJSON = genericToJSON defaultOptions { fieldLabelModifier = upperFirstChar }

fromCamalJSON :: (Generic a, GFromJSON (Rep a)) => Value -> Parser a
fromCamalJSON = genericParseJSON defaultOptions { fieldLabelModifier = upperFirstChar }
