{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.ByteString (ByteString)
import Data.Word (Word8)
import GHC.IO.Handle (Handle)
import Streamly
import Streamly.FileSystem.Handle (readChunks)
import Streamly.Memory.Array (Array)
import System.IO (openFile, IOMode(ReadMode))
import System.IO.Temp (withSystemTempFile)
import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BSL
import Streamly.External.FileSystem.Handle.Posix
import qualified Streamly.Prelude as S

checkCopyLBS :: FilePath -> Handle -> IO ()
checkCopyLBS filename handle' = do
  let content = "Some file content"
  print $ "Checking " <> filename
  handle <- openFile filename ReadMode
  copyLBS content handle
  bsContent <- BS.hGetContents handle'
  bsContent `shouldBe` content


main :: IO ()
main =
  hspec $ do
    describe "Streamly.External.FileSystem.Handle.Posix" $ do
      it "copyLBS" $
        withSystemTempFile "x" checkCopyLBS


