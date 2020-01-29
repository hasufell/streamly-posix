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
module Streamly.External.FileSystem.Handle.Posix
  (
  -- * File reading
    readFileLBS
  , readFileStream
  -- * File writing
  , copyFileHandle
  , copyFileStream
  -- * Directory listing
  , unfoldDirContents
  , dirContentsStream
  , dirContents
  , DirType
  )
where

import           Control.Exception.Safe
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Data.Word                      ( Word8 )
import           Data.Word8
import           Prelude                 hiding ( readFile )
import           Streamly
import           Streamly.Internal.Data.Unfold.Types
import           Streamly.Memory.Array
import           System.IO                      ( Handle
                                                , hClose
                                                )
import           System.IO.Unsafe
import           System.Posix.ByteString
import           System.Posix.Directory.ByteString
                                               as PosixBS
import           System.Posix.Foreign           ( DirType )
import           System.Posix.RawFilePath.Directory.Traversals
                                         hiding ( getDirectoryContents )
import qualified Data.ByteString               as BS
import qualified Data.ByteString.Lazy          as L
import qualified Data.ByteString.Lazy.Internal as BSLI
import qualified Streamly.External.ByteString  as Strict
import qualified Streamly.FileSystem.Handle    as FH
import qualified Streamly.Internal.Data.Stream.StreamD.Type
                                               as D
import qualified Streamly.Internal.Data.Unfold as SIU
import qualified Streamly.Internal.Prelude     as S


-- |Read the given file lazily as a lazy ByteString.
--
-- The handle is closed automatically, when the stream exits normally,
-- aborts or gets garbage collected.
--
-- This uses `unsafeInterleaveIO` under the hood.
readFileLBS :: Handle  -- ^ readable file handle
            -> IO L.ByteString
readFileLBS handle' = fromChunks (readFileStream handle')
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
readFileStream = S.unfold (SIU.finallyIO (liftIO . hClose) FH.readChunks)


-- | Like 'copyFileStream', except for two file handles.
copyFileHandle :: (MonadCatch m, MonadAsync m, MonadMask m)
               => Handle  -- ^ copy from this handle, must be readable
               -> Handle  -- ^ copy to this handle, must be writable
               -> m ()
copyFileHandle fromHandle toHandle =
  copyFileStream (readFileStream fromHandle) toHandle


-- | Copy a stream to a file handle.
--
-- The handle is closed automatically after the stream is copied.
copyFileStream :: (MonadCatch m, MonadAsync m, MonadMask m)
               => SerialT m (Array Word8) -- ^ stream to copy
               -> Handle                  -- ^ file handle to copy to, must be writable
               -> m ()
copyFileStream stream handle' =
  (flip finally) (liftIO $ hClose handle')
    $ S.fold (FH.writeChunks handle') stream


-- | Create an 'Unfold' of directory contents.
unfoldDirContents :: MonadIO m => Unfold m DirStream (DirType, RawFilePath)
unfoldDirContents = Unfold step return
 where
  {-# INLINE [0] step #-}
  step dirstream = do
    (typ, e) <- liftIO $ readDirEnt dirstream
    return if
      | BS.null e                       -> D.Stop
      | BS.pack [_period] == e          -> D.Skip dirstream
      | BS.pack [_period, _period] == e -> D.Skip dirstream
      | otherwise                       -> D.Yield (typ, e) dirstream


-- | Read the directory contents as a stream.
--
-- The DirStream is closed automatically, when the streamly stream exits
-- normally, aborts or gets garbage collected.
dirContentsStream :: (MonadCatch m, MonadAsync m, MonadMask m)
                  => DirStream
                  -> SerialT m (DirType, RawFilePath)
dirContentsStream =
  S.unfold (SIU.finallyIO (liftIO . PosixBS.closeDirStream) unfoldDirContents)


-- | Read the directory contents strictly as a list.
--
-- The DirStream is closed automatically.
dirContents :: (MonadCatch m, MonadAsync m, MonadMask m)
            => DirStream
            -> m [(DirType, RawFilePath)]
dirContents = S.toList . dirContentsStream
