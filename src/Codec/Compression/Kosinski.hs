module Codec.Compression.Kosinski (
  Compressed(..)
, compressed
, compressedFile
, decompress
) where

import           Control.Lens
import           Data.Bits
import           Data.Bits.Lens
import qualified Data.ByteString  as BS
import           Data.Maybe
import           Data.Semigroup   ((<>))
import qualified Data.Vector      as V
import           Data.Vector.Lens
import           Data.Word

data Compressed
  = Compressed Word8 Word8 [Word8]
  deriving (Eq, Ord, Show)

compressed :: BS.ByteString -> Maybe Compressed
compressed bs = do
  (a, bs') <- BS.uncons bs
  (b, bs'') <- BS.uncons bs'
  pure . Compressed a b $ BS.unpack bs''

compressedFile :: FilePath -> IO (Maybe Compressed)
compressedFile fp =
  compressed <$> BS.readFile fp

decompress :: Compressed -> Maybe [Word8]
decompress (Compressed h1 h2 s) =
  V.toList <$> f (headerBits h1 h2) s mempty
  where
    -- Uncompressed
    f (True:x:xs) (a:as) d =
      f (x:xs) as $ V.snoc d a

    -- End of header
    f (x:y:[]) (a:b:as) d =
      f (x:y:headerBits a b) as d

    -- Full
    f (False:True:xs) (l:h:as) d = do
      let h' = fromIntegral h
          count = h' .&. 0x7 :: Int
          l' = fromIntegral l
          h'' = shiftL (0xF8 .&. h') 5
          offset = complement 0x1FFF .|. h'' .|. l'

      if count == 0
        then do
          count' <- fromIntegral <$> listToMaybe as
          if count' == 0
            then pure d
            else if count' == 1
                 then f xs (drop 1 as) d
                 else f xs (drop 1 as) $ dict offset count' d
        else f xs as $ dict offset (count + 1) d

    -- End of header
    f (w:x:y:z:[]) (a:b:as) d =
      f (w:x:y:z:headerBits a b) as d

    -- Inline
    f (False:False:l:h:xs) (o:as) d = do
      let count = (if l then 2 else 0) + (if h then 1 else 0) + 1
          offset = fromIntegral o .|. complement 0xFF
      f xs as (dict offset count d)

    -- End of header
    f h (a:b:as) d =
      f (h ++ headerBits a b) as d

    -- End of data
    f _ [] c =
      pure c

    -- Unknown data
    f _ _ _ =
      Nothing

dict :: Int -> Int -> V.Vector a -> V.Vector a
dict offset count d =
  d <> toVectorOf (taking (count + 1) (repeated . dropping i traverse)) d
  where
    i =
      length d + offset

headerBits :: (Bits b, Num b) => b -> b -> [Bool]
headerBits =
  curry . toListOf $ both . bits
