{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.ByteString                ( ByteString )
import           Data.Word                      ( Word8 )
import           GHC.IO.Handle                  ( Handle )
import           Streamly
import           Streamly.FileSystem.Handle     ( readChunks )
import           Streamly.Memory.Array          ( Array )
import           System.IO                      ( openFile
                                                , IOMode(ReadMode)
                                                )
import           System.IO.Temp                 ( withSystemTempFile )
import           Test.Hspec
import           Test.Hspec.QuickCheck

import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           Streamly.External.FileSystem.Handle.Posix
import qualified Streamly.Prelude              as S

checkCopyLBS :: FilePath -> Handle -> IO ()
checkCopyLBS filename handle' = do
  let content = "Some file content"
  print $ "Checking " <> filename
  copyLBS content handle'
  handle    <- openFile filename ReadMode
  bsContent <- BS.hGetContents handle
  bsContent `shouldBe` (BSL.toStrict content)

checkReadFileLBS :: FilePath -> Handle -> IO ()
checkReadFileLBS filename handle' = do
  let content = "Some file content"
  print $ "Checking " <> filename
  copyLBS content handle'
  handle    <- openFile filename ReadMode
  bsContent <- readFileLBS handle
  (BSL.toStrict bsContent) `shouldBe` (BSL.toStrict content)

checkCopyFileHandle :: Handle -> Handle -> IO ()
checkCopyFileHandle from to = do
  let content = "Some file content"
  copyLBS' content from
  copyFileHandle' from to
  fromContent <- readFileLBS from
  toContent <- readFileLBS to
  (BSL.toStrict fromContent) `shouldBe` (BSL.toStrict toContent)


main :: IO ()
main = hspec $ do
  describe "Streamly.External.FileSystem.Handle.Posix" $ do
    it "copyLBS" $ withSystemTempFile "x" checkCopyLBS
    it "readFileLBS" $ withSystemTempFile "x" checkReadFileLBS
    it "copyFileHandle" $ withSystemTempFile
      "x"
      (\_ h1 -> withSystemTempFile "y" $ (\_ h2 -> checkCopyFileHandle h1 h2))
