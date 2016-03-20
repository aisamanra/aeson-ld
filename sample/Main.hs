{-# LANGUAGE OverloadedLists #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Aeson
import           Data.Aeson.Encode.Pretty (encodePretty)
import           Data.Aeson.Linked
import           Data.Aeson.Linked.Builder
import qualified Data.ByteString.Lazy.Char8 as BS

name, url, image :: LDField
name  = field "name"  "http://schema.org/name"
url   = field "url"   "http://schema.org/url"   `withType` "@id"
image = field "image" "http://schema.org/image" `withType` "@id"

jsPrint :: Value -> IO ()
jsPrint = BS.putStrLn . encodePretty

main :: IO ()
main = do
  let fields =
        [ ( name,  "Getty Ritter" )
        , ( url,   "http://gdritter.com/" )
        , ( image, "http://gdritter.com/imgs/gdritter.jpg" )
        ]
      doc = unlinkedObject fields
      ctx = ldContext [name, url, image]

  jsPrint doc
  jsPrint ctx

  let compacted = compact doc ctx
  jsPrint compacted

  let expanded = expand compacted
  jsPrint expanded

  let flattened = flatten doc
  jsPrint flattened

  let framed = frame doc
  jsPrint framed

  let normalized = normalize doc
  jsPrint normalized
