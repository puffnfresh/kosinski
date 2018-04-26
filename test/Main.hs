{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Codec.Compression.Kosinski
import           Control.Exception
import           Data.ByteString
import           Data.Word
import           Test.Tasty
import           Test.Tasty.HUnit

-- Tests from https://segaretro.org/Kosinski_compression

uncompressedData :: ByteString
uncompressedData =
  "\xFF\x5F\x00\x01\x02\x03\x04\x05\x06\x07\x08\x09\x0A\x0B\x0C\x00\xF0\x00"

uncompressedDataExpected :: [Word8]
uncompressedDataExpected =
  [0x00, 0x01, 0x02, 0x03, 0x04, 0x05, 0x06, 0x07, 0x08, 0x09, 0x0A, 0x0B, 0x0C]

inlineDictionaryMatch :: ByteString
inlineDictionaryMatch =
  "\x51\x00\x25\xFF\x00\xF0\x00"

inlineDictionaryMatchExpected :: [Word8]
inlineDictionaryMatchExpected =
  [0x25, 0x25, 0x25, 0x25]

finalExample :: ByteString
finalExample =
  "\xFF\x3F\x54\x3B\xC4\x44\x54\x33\x33\x5B\x2D\x5C\x44\x5C\xC4\xC5\xFC\x15\xFE\xC3\x44\x78\x88\x98\x44\x30\xFF\xFF\x00\xF8\x00"

finalExampleExpected :: [Word8]
finalExampleExpected =
  [0x54, 0x3B, 0xC4, 0x44, 0x54, 0x33, 0x33, 0x5B, 0x2D, 0x5C, 0x44, 0x5C, 0xC4, 0xC5, 0xC4, 0xC5, 0xC3, 0x44, 0x78, 0x88, 0x98, 0x44, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30, 0x30]

decompressByteString :: ByteString -> Maybe [Word8]
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
