{-# LANGUAGE CPP               #-}
{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Foldable
import           Data.List                      ( sortBy )
import           GHC.IO.Handle                  ( Handle )
import           System.IO
import           System.IO.Temp
import           Test.Hspec
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as BSL
import           Streamly.External.FileSystem.Handle
import System.FilePath
#if !defined(mingw32_HOST_OS)
import           Streamly.External.FileSystem.DirStream.Posix
import           System.Posix.Directory as Posix
import           System.Posix.Foreign



checkDirContents :: FilePath -> IO ()
checkDirContents fp = do
  let f1 = fp </> "f1"
  let f2 = fp </> "f2"
  let f3 = fp </> "f3"
  let f4 = fp </> "f4"
  for_ [f1, f2, f3, f4] $ \f -> openFile f ReadWriteMode
  ds       <- Posix.openDirStream fp
  contents <- fmap (sortBy (\(_, y) (_, z) -> compare y z)) $ dirContents ds
  contents
    `shouldBe` [ (DirType 8, "f1")
               , (DirType 8, "f2")
               , (DirType 8, "f3")
               , (DirType 8, "f4")
               ]
#endif


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
  toContent   <- readFileLBS to
  (BSL.toStrict fromContent) `shouldBe` (BSL.toStrict toContent)


main :: IO ()
main = hspec $ do
  describe "Streamly.External.FileSystem.Handle.Posix" $ do
    it "copyLBS" $ withSystemTempFile "x" checkCopyLBS
    it "readFileLBS" $ withSystemTempFile "x" checkReadFileLBS
    it "copyFileHandle" $ withSystemTempFile
      "x"
      (\_ h1 -> withSystemTempFile "y" $ (\_ h2 -> checkCopyFileHandle h1 h2))
#if !defined(mingw32_HOST_OS)
    it "dirContents" $ withSystemTempDirectory "y" checkDirContents
#endif
