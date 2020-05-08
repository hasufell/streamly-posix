{-# LANGUAGE CPP              #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiWayIf       #-}

-- |
-- Module      :  Streamly.External.Posix.DirStream
-- Copyright   :  © 2020 Julian Ospald
-- License     :  BSD3
--
-- Maintainer  :  Julian Ospald <hasufell@posteo.de>
-- Stability   :  experimental
-- Portability :  portable
--
-- This module provides high-level file streaming API,
-- working with directory streams (POSIX).
module Streamly.External.Posix.DirStream
  (
  -- * Directory listing
    unfoldDirContents
  , dirContentsStream
  , dirContents
  )
where

import           Control.Exception.Safe
import           Control.Monad.IO.Class         ( liftIO
                                                , MonadIO
                                                )
import           Data.Word8
import           Prelude                 hiding ( readFile )
import           Streamly
import           Streamly.Internal.Data.Unfold.Types
import           System.Posix.ByteString
import           System.Posix.Directory.ByteString
                                               as PosixBS
import           System.Posix.Foreign           ( DirType )
import           System.Posix.RawFilePath.Directory.Traversals
                                         hiding ( getDirectoryContents )
import qualified Data.ByteString               as BS
import qualified Streamly.Internal.Data.Stream.StreamD.Type
                                               as D
#if MIN_VERSION_streamly(0,7,1)
import qualified Streamly.Internal.Data.Unfold as SIU
#endif
import qualified Streamly.Internal.Prelude     as S


-- | Create an 'Unfold' of directory contents.
unfoldDirContents :: MonadIO m => Unfold m DirStream (DirType, RawFilePath)
unfoldDirContents = Unfold step return
 where
  {-# INLINE [0] step #-}
  step dirstream = do
    (typ, e) <- liftIO $ readDirEnt dirstream
    return $ if
      | BS.null e                       -> D.Stop
      | BS.pack [_period] == e          -> D.Skip dirstream
      | BS.pack [_period, _period] == e -> D.Skip dirstream
      | otherwise                       -> D.Yield (typ, e) dirstream


-- | Read the directory contents as a stream.
--
-- The DirStream is closed automatically, when the streamly stream exits
-- normally, aborts or gets garbage collected.
-- The stream must not be used after the dirstream is closed.
dirContentsStream :: (MonadCatch m, MonadAsync m, MonadMask m)
                  => DirStream
                  -> SerialT m (DirType, RawFilePath)
dirContentsStream ds =
#if MIN_VERSION_streamly(0,7,1)
  S.unfold (SIU.finallyIO (liftIO . PosixBS.closeDirStream) unfoldDirContents) $ ds
#else
  S.finally (liftIO . PosixBS.closeDirStream $ ds) . S.unfold unfoldDirContents $ ds
#endif


-- | Read the directory contents strictly as a list.
--
-- The DirStream is closed automatically.
dirContents :: (MonadCatch m, MonadAsync m, MonadMask m)
            => DirStream
            -> m [(DirType, RawFilePath)]
dirContents = S.toList . dirContentsStream

