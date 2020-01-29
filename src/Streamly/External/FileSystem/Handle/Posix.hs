{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

-- |
-- Module      :  Streamly.External.FileSystem.Handle.Posix
-- Copyright   :  © 2020 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides high-level file streaming API.
module Streamly.External.FileSystem.Handle.Posix where

import           Streamly
import           Streamly.Memory.Array
import qualified Streamly.Memory.Array         as A
import qualified Streamly.FileSystem.Handle    as FH
import qualified Streamly.Internal.FileSystem.Handle
                                               as IFH
import qualified Streamly.Prelude              as S
import qualified Streamly.Internal.Prelude     as S
import           System.IO                      ( Handle
                                                , hClose
                                                )
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Data.Word                      ( Word8 )
import           Data.Word8
import           Control.Exception.Safe
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString               as BS
import qualified Streamly.External.ByteString  as Strict
import qualified Data.ByteString.Lazy.Internal as BSLI
import           System.IO.Unsafe
import qualified Streamly.Internal.Data.Unfold as SIU
import           Streamly.Internal.Data.Unfold.Types
import           System.Posix.RawFilePath.Directory.Traversals
import qualified Streamly.Internal.Data.Stream.StreamD.Type
                                               as D
import           System.Posix.ByteString
import           System.Posix.Foreign           ( DirType )
import           System.Posix.Directory.ByteString
                                               as PosixBS


-- |Read the given file lazily as a lazy ByteString.
--
-- The handle is closed automatically, when the stream exits normally,
-- aborts or gets garbage collected.
--
-- This uses `unsafeInterleaveIO` under the hood.
readFile :: Handle -> IO L.ByteString
readFile handle' = fromChunks (readFileStream handle')
 where
  -- https://github.com/psibi/streamly-bytestring/issues/7
  fromChunks =
    S.foldrM (\x b -> unsafeInterleaveIO b >>= pure . BSLI.chunk x)
             (pure BSLI.Empty)
      . S.map Strict.fromArray


-- | Read from the given handle as a streamly filestream.
--
-- The handle is closed automatically, when the stream exits normally,
-- aborts or gets garbage collected.
readFileStream :: (MonadCatch m, MonadAsync m)
               => Handle
               -> SerialT m (Array Word8)
readFileStream = S.unfold
  (SIU.finallyIO (liftIO . hClose)
                 FH.readChunks
  )


-- | Like 'copyFileStream', except for two file handles.
copyFile :: (MonadCatch m, MonadAsync m, MonadMask m)
         => Handle
         -> Handle
         -> m ()
copyFile fromHandle toHandle =
  copyFileStream (readFileStream fromHandle) toHandle


-- | Copy a stream to a file handle.
--
-- The handle is closed automatically after the stream is copied.
copyFileStream :: (MonadCatch m, MonadAsync m, MonadMask m)
               => SerialT m (Array Word8) -- ^ stream to copy
               -> Handle                  -- ^ file handle to copy to
               -> m ()
copyFileStream stream handle' =
  finally (liftIO $ hClose handle') $ S.fold (FH.writeChunks handle') stream


unfoldDirectoryContents :: MonadIO m
                        => Unfold m DirStream (DirType, RawFilePath)
unfoldDirectoryContents = Unfold step return
 where
  {-# INLINE [0] step #-}
  step dirstream = do
    (typ, e) <- liftIO $ readDirEnt dirstream
    return if
      | BS.null e                       -> D.Stop
      | BS.pack [_period]          == e -> D.Skip dirstream
      | BS.pack [_period, _period] == e -> D.Skip dirstream
      | otherwise                       -> D.Yield (typ, e) dirstream


getDirectoryContents :: (MonadCatch m, MonadAsync m, MonadMask m)
                     => DirStream
                     -> SerialT m (DirType, RawFilePath)
getDirectoryContents = S.unfold
  (SIU.finallyIO (liftIO . PosixBS.closeDirStream) unfoldDirectoryContents)
