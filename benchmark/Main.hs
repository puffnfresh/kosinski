module Main where

import           Codec.Compression.Kosinski (compressedFile, decompress)
import           Control.DeepSeq            (deepseq)
import qualified Data.ByteString            as BS
import           Data.Time.Clock            (diffUTCTime, getCurrentTime)
import           Data.Word                  (Word8)
import           System.FilePath            ((</>))

main :: IO ()
main = do
  -- Large file generated via KENSSharp
  Just bin <- compressedFile ("benchmark" </> "haskell-logo-png.bin")
  start <- getCurrentTime
  deepseq (decompress bin) $ return ()
  end <- getCurrentTime
  putStrLn $ "Kosinski decompress took " ++ show (diffUTCTime end start)
