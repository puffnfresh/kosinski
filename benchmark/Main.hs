module Main where

import           Codec.Compression.Kosinski (compressed, decompress)
import           Control.DeepSeq            (deepseq)
import qualified Data.ByteString            as BS
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Word                  (Word8)
import           System.FilePath            ((</>))

decompressByteString :: BS.ByteString -> Maybe [Word8]
decompressByteString =
  (>>= decompress) . compressed

main :: IO ()
main = do
  -- Large file generated via KENSSharp
  bin <- BS.readFile ("benchmark" </> "haskell-logo-png.bin")
  start <- getCurrentTime
  deepseq (decompressByteString bin) $ return ()
  end <- getCurrentTime
  putStrLn $ "Kosinski decompress took " ++ show (diffUTCTime end start)
