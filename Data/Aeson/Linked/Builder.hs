{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Data.Aeson.Linked.Builder where

import           Data.Aeson
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Vector (fromList)

data LDField = LDField
  { fiName  :: Text
  , fiIRI   :: Text
  , fiType  :: Maybe Text
  } deriving (Eq, Show)

unlinkedObject :: [(LDField, Value)] -> Value
unlinkedObject is =
  object [ (name, value) | (LDField { fiName = name }, value) <- is ]

-- | Create a compact JSON-LD document with the provided key-value pairs.
ldObject :: [(LDField, Value)] -> Value
ldObject is = object ( ("@context", ldContext (map fst is)) : fs )
  where fs  = [ (fiName info, value) | (info, value) <- is ]

ldObjectExpanded :: [(LDField, Value)] -> Value
ldObjectExpanded  is =
  object [ ( fiIRI
           , Array $ fromList
             [ case fiType of
                Just typ -> object [ (typ, val) ]
                Nothing  -> object [ ("@value", val) ]
             ]
           )
         | (LDField { .. }, val) <- is
         ]

ldContext :: [LDField] -> Value
ldContext is = object [ (fiName info, mkCtx info) | info <- is ]
  where mkCtx LDField { .. }
          | Nothing  <- fiType = String fiIRI
          | Just typ <- fiType =
              object [ ("@id", String fiIRI), ("@type", String typ) ]

-- | Create a representation of a Linked Data field, which must contain
--   both a name and an associated IRI.
field :: Text -> Text -> LDField
field fiName fiIRI = LDField fiName fiIRI Nothing

-- | Set the "type" field of a context.
withType :: LDField -> Text -> LDField
withType f typ = f { fiType = Just typ }
