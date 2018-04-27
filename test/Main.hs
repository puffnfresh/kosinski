{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Codec.Compression.Kosinski
import           Data.ByteString
import           Test.Tasty
import           Test.Tasty.HUnit

-- Tests from https://segaretro.org/Kosinski_compression

uncompressedData :: ByteString
uncompressedData =
  "\xFF\x5F\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x00\xF0\x00"

uncompressedDataExpected :: ByteString
uncompressedDataExpected =
  "\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C"

inlineDictionaryMatch :: ByteString
inlineDictionaryMatch =
  "\x51\x00\x25\xFF\x00\xF0\x00"

inlineDictionaryMatchExpected :: ByteString
inlineDictionaryMatchExpected =
  "\x25\x25\x25\x25"

finalExample :: ByteString
finalExample =
  "\xFF\x3F\x54\x3B\xC4\x44\x54\x33\x33\x5B\x2D\x5C\x44\x5C\xC4\xC5\xFC\x15\xFE\xC3\x44\x78\x88\x98\x44\x30\xFF\xFF\x00\xF8\x00"

finalExampleExpected :: ByteString
finalExampleExpected =
  "\x54\x3B\xC4\x44\x54\x33\x33\x5B\x2D\x5C\x44\x5C\xC4\xC5\xC4\xC5\xC3\x44\x78\x88\x98\x44\x30\x30\x30\x30\x30\x30\x30\x30\x30\x30"

decompressByteString :: ByteString -> Maybe ByteString
decompressByteString =
  (>>= decompress) . compressed

tests :: TestTree
tests =
  testGroup "Sega Retro tests"
    [ testCase "Uncompressed Data" $
        decompressByteString uncompressedData @?= Just uncompressedDataExpected
    , testCase "Inline dictionary match" $
        decompressByteString inlineDictionaryMatch @?= Just inlineDictionaryMatchExpected
    , testCase "Final example" $
        decompressByteString finalExample @?= Just finalExampleExpected
    ]

main :: IO ()
main =
  defaultMain tests
