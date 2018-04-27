{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Codec.Compression.Kosinski (
  Compressed(..)
, compressed
, compressedFile
, decompress
) where

import           Control.Lens
import           Control.Monad           (unless, when)
import           Control.Monad.Except    (MonadError, throwError)
import           Control.Monad.State     (MonadState, execStateT)
import           Data.Bits
import qualified Data.ByteString         as BS
import           Data.ByteString.Builder
import qualified Data.ByteString.Lazy    as BSL
import           Data.Int                (Int64)
import           Data.Maybe              (fromMaybe, maybe)
import           Data.Semigroup          (stimes, (<>))
import           Data.Word               (Word16, Word8)

data Compressed
  = Compressed Int Word16 BS.ByteString
  deriving (Eq, Ord, Show)

data DecompressState
  = DecompressState Builder Compressed

builder_ :: Lens' DecompressState Builder
builder_ =
  lens f g
  where
    f (DecompressState a _) =
      a
    g (DecompressState _ b) a =
      DecompressState a b

class HasCompressed m where
  compressed_ :: Lens' m Compressed

instance HasCompressed Compressed where
  compressed_ =
    id

instance HasCompressed DecompressState where
  compressed_ =
    lens f g
    where
      f (DecompressState _ b) =
        b
      g (DecompressState a _) b =
        DecompressState a b

unconsWord16 :: BS.ByteString -> Maybe (Word16, BS.ByteString)
unconsWord16 bs = do
  (a, bs') <- BS.uncons bs
  (b, bs'') <- BS.uncons bs'
  pure ((fromIntegral b `shiftL` 8) .|. fromIntegral a, bs'')

compressedFile :: FilePath -> IO (Maybe Compressed)
compressedFile =
  fmap compressed . BS.readFile

compressed :: BS.ByteString -> Maybe Compressed
compressed =
  fmap f . unconsWord16
  where
    f (b, bs) =
      Compressed 0 b bs

nextIsNull :: (HasCompressed s, MonadState s m) => m Bool
nextIsNull = do
  Compressed _ _ bs <- use compressed_
  pure $ BS.null bs

nextBit :: (HasCompressed s, MonadError () m, MonadState s m) => m Bool
nextBit = do
  Compressed r b bs <- use compressed_
  let
    r' =
      r + 1
    c =
      fromMaybe (Compressed 0 0 mempty) (compressed bs)
    c' =
      if (r' > 0xF)
      then c
      else Compressed r' b bs
  compressed_ .= c'
  pure $ testBit b r

nextByte :: (HasCompressed s, MonadError () m, MonadState s m) => m Word8
nextByte = do
  Compressed r b bs <- use compressed_
  (b', bs') <- maybe (throwError ()) pure $ BS.uncons bs
  compressed_ .= Compressed r b bs'
  pure b'

checkEndOfStream :: (HasCompressed s, MonadState s m) => m () -> m ()
checkEndOfStream m = do
  n <- nextIsNull
  unless n m

tell :: MonadState DecompressState m => Builder -> m ()
tell =
  (builder_ <>=)

modifyByteString :: MonadState DecompressState m => (BSL.ByteString -> Builder) -> m ()
modifyByteString f =
  builder_ %= \b -> b <> f (toLazyByteString b)

handleUncompressed :: (MonadError () m, MonadState DecompressState m) => m ()
handleUncompressed = do
  b <- nextByte
  tell $ word8 b
  decompress'

dictBuilder :: Int64 -> Int64 -> BSL.ByteString -> Builder
dictBuilder offset count d =
  lazyByteString c
  where
    c =
      BSL.take (count + 1) . stimes (count + 1) $ BSL.drop i d
    i =
      BSL.length d + offset

handleFull :: (MonadError () m, MonadState DecompressState m) => m ()
handleFull = do
  l <- nextByte
  h <- nextByte
  let h' = fromIntegral h
      count = h' .&. 0x7 :: Int64
      l' = fromIntegral l
      h'' = shiftL (0xF8 .&. h') 5
      offset = complement 0x1FFF .|. h'' .|. l'
  if count == 0
  then checkEndOfStream $ do
    count' <- nextByte
    when (count' /= 0) $
      if count' == 1
      then decompress'
      else do
        modifyByteString . dictBuilder offset $ fromIntegral count'
        decompress'
  else do
    modifyByteString . dictBuilder offset $ count + 1
    decompress'

handleInline :: (MonadError () m, MonadState DecompressState m) => m ()
handleInline = do
  l <- nextBit
  h <- nextBit
  o <- nextByte
  let count = (if l then 2 else 0) + (if h then 1 else 0) + 1
      offset = fromIntegral o .|. complement 0xFF
  modifyByteString $ dictBuilder offset count
  decompress'

decompress' :: (MonadError () m, MonadState DecompressState m) => m ()
decompress' = checkEndOfStream $ do
  isUncompressed <- nextBit
  if isUncompressed
  then handleUncompressed
  else checkEndOfStream $ do
    isFull <- nextBit
    if isFull
    then handleFull
    else handleInline

decompress :: Compressed -> Maybe BS.ByteString
decompress =
  fmap (BSL.toStrict . toLazyByteString . view builder_) . execStateT decompress' . DecompressState mempty
