{-# LANGUAGE RecordWildCards #-}

module Data.Aeson.Linked where

import Data.Aeson

data CompactOpts = CompactOpts
  { coBase          :: String
  , coCompactArrays :: Bool
  , coGraph         :: Bool
  , coSkipExpension :: Bool
  , coActiveCtx     :: Bool
  , coLink          :: Bool
  } deriving (Eq, Show)

defaultCompactOpts :: CompactOpts
defaultCompactOpts = CompactOpts
  { coBase          = ""
  , coCompactArrays = True
  , coGraph         = False
  , coSkipExpension = False
  , coActiveCtx     = False
  , coLink          = False
  }

compact :: Value -> Value -> Value
compact = compactWithOpts defaultCompactOpts

compactWithOpts :: CompactOpts -> Value -> Value -> Value
compactWithOpts = undefined

data ExpandOpts = ExpandOpts
  { eoKeepFreeFloatingNodes :: Bool
  } deriving (Eq, Show)

defaultExpandOpts :: ExpandOpts
defaultExpandOpts = ExpandOpts
  { eoKeepFreeFloatingNodes = False
  }

expand :: Value -> Value
expand = expandWithOpts defaultExpandOpts

expandWithOpts :: ExpandOpts -> Value -> Value
expandWithOpts ExpandOpts { .. } = undefined

flatten :: Value -> Value
flatten = undefined

frame :: Value -> Value
frame = undefined

normalize :: Value -> Value
normalize = undefined
