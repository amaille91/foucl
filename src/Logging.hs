module Logging (log) where

import Prelude hiding (log)
import Control.Monad.IO.Class (MonadIO, liftIO)
import System.IO (hFlush, stdout)

log :: (Show s, MonadIO m) => s -> m ()
log s = liftIO $ print s >> hFlush stdout
