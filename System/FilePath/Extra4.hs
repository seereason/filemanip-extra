-- | Was in Debian.Repo.MonadOS
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables #-}
module System.FilePath.Extra4
    ( withMount
    , WithProcAndSys
    , withProcAndSys
    , withTmp
    ) where

import Control.Applicative (Applicative)
import Control.Exception (catch)
import Control.Monad.Catch (bracket, MonadCatch, MonadMask)
import Control.Monad.Trans (MonadTrans, lift, liftIO, MonadIO)
-- import Control.Monad.Trans.Except ({- ExceptT instances -})
import Data.ByteString.Lazy as L (ByteString, empty)
import GHC.IO.Exception (IOErrorType(OtherError))
import System.Directory (createDirectoryIfMissing)
import System.Exit (ExitCode(ExitFailure, ExitSuccess))
import System.FilePath ((</>))
import System.IO (hPutStrLn, stderr)
import System.IO.Error
import System.Process (CreateProcess, proc)
import System.Process.ListLike (readCreateProcess, showCreateProcessForUser)

readProcess :: CreateProcess -> L.ByteString -> IO L.ByteString
readProcess p input = do
  (code, out, _err) <- readCreateProcess p input :: IO (ExitCode, L.ByteString, L.ByteString)
  case code of
    ExitFailure n -> ioError (mkIOError OtherError (showCreateProcessForUser p ++ " -> " ++ show n) Nothing Nothing)
    ExitSuccess -> return out

-- | Do an IO task with a file system remounted using mount --bind.
-- This was written to set up a build environment.
withMount :: forall m c. (MonadIO m, MonadCatch m, MonadMask m) => FilePath -> FilePath -> m c -> m c
withMount directory mountpoint task =
    bracket pre (\ _ -> post) (\ _ -> task)
    where
      mount = proc "mount" ["--bind", directory, mountpoint]
      umount = proc "umount" [mountpoint]
      umountLazy = proc "umount" ["-l", mountpoint]
      pre :: m L.ByteString
      pre = liftIO $ do createDirectoryIfMissing True mountpoint
                        readProcess mount L.empty
      post :: m L.ByteString
      post = liftIO $ do readProcess umount L.empty
                           `catch` (\ (e :: IOError) ->
                                        do hPutStrLn stderr ("Exception unmounting " ++ mountpoint ++ ", trying -l: " ++ show e)
                                           readProcess umountLazy L.empty)

newtype WithProcAndSys m a = WithProcAndSys { runWithProcAndSys :: m a } deriving (Functor, Monad, Applicative)

instance MonadTrans WithProcAndSys where
    lift = WithProcAndSys

instance MonadIO m => MonadIO (WithProcAndSys m) where
    liftIO task = WithProcAndSys (liftIO task)

withProcAndSys :: forall m c. (MonadIO m, MonadCatch m, MonadMask m) => FilePath -> WithProcAndSys m c -> m c
withProcAndSys "/" task = runWithProcAndSys task -- If root is / these should already be mounted
withProcAndSys root task =
    withMount "/proc" (root </> "proc") $ withMount "/sys" (root </> "sys") $ runWithProcAndSys task

-- | Do an IO task with /tmp remounted.  This could be used
-- to share /tmp with a build root.
withTmp :: forall m c. (MonadIO m, MonadCatch m, MonadMask m) => FilePath -> m c -> m c
withTmp root task = withMount "/tmp" (root </> "tmp") task
