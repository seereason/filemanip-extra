-- | Was Appraisal.Utils.Files in the image-cache package.
{-# LANGUAGE CPP, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wall #-}
module System.FilePath.Extra
    ( UpdateResult(..)
    , updateFile
    , createFile
    , replaceFile
    , removeFileIfPresent
    , writeFileReadable
    , makeReadableAndClose
    , compareSaveAndReturn
    , changeError
    ) where

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative ((<$>))
#endif
import Control.Exception as E (catch, IOException, throw, try)
import Data.ListLike as LL (hPutStr, ListLikeIO, readFile, writeFile)
import GHC.IO.Exception (ioe_description)
import Prelude hiding (readFile)
import System.Directory (removeFile)
import qualified System.IO as IO
import System.IO.Error (isDoesNotExistError)
import System.Posix.Files (getFdStatus, fileMode, setFdMode, unionFileModes, ownerReadMode, groupReadMode, otherReadMode)
import System.Posix.IO (handleToFd, closeFd)

data UpdateResult = Unchanged | Created | Modified deriving (Eq, Ord, Read, Show)

-- | Write a file if its content is different from the given text.
updateFile :: forall full item. (ListLikeIO full item, Eq full) => FilePath -> full -> IO UpdateResult
updateFile path text =
    createFile path text >>= \r ->
        case r of
          Modified -> do replaceFile path text
                         return Modified
          _ -> return r

-- | Like updateFile, but doesn't write the file if it needs to be
-- modified.  Returns the same UpdateResult as updateFile.  This
-- *will* (attempt to) write the file if it doesn't exist.
createFile :: forall full item. (ListLikeIO full item, Eq full) => FilePath -> full -> IO UpdateResult
createFile path text =
    try (readFile path) >>= maybeWrite
    where
      maybeWrite :: Either IOException full -> IO UpdateResult
      maybeWrite (Left (e :: IOException)) | isDoesNotExistError e = writeFileReadable path text >> return Created
      maybeWrite (Left e) = throw (e {ioe_description = ioe_description e ++ " (via System.FilePath.Extra.createFile) "})
      maybeWrite (Right old) | old == text = return Unchanged
      maybeWrite (Right _old) =
          do --hPutStrLn stderr ("Old text: " ++ show old) >>
             --hPutStrLn stderr ("New text: " ++ show text) >>
             return Modified

-- | Replace a file's contents, accounting for the possibility that the
-- old contents of the file may still be being read.  Apparently there
-- is (was) a race condition in the file system so we may get one or more
-- isAlreadyBusyError exceptions before the writeFile succeeds.
replaceFile :: forall full item. (ListLikeIO full item, Eq full) => FilePath -> full -> IO ()
replaceFile path text =
    --tries 100 10 $ -- (This problem was fixed, see https://ghc.haskell.org/trac/ghc/ticket/2122)
    removeFileIfPresent path >> writeFileReadable path text

removeFileIfPresent :: FilePath -> IO ()
removeFileIfPresent path = removeFile path `E.catch` (\ e -> if isDoesNotExistError e then return () else ioError e)

-- | Write a file and make it readable
writeFileReadable :: forall full item. (ListLikeIO full item, Eq full) => FilePath -> full -> IO ()
writeFileReadable path bytes = do
  fp <- IO.openFile path IO.WriteMode
  hPutStr fp bytes
  makeReadableAndClose fp

makeReadableAndClose :: IO.Handle -> IO ()
makeReadableAndClose fp = do
  -- This closes the handle (but not the fd)
  fd <- handleToFd fp
  mode <- fileMode <$> getFdStatus fd
  let mode' = foldr unionFileModes mode [ownerReadMode, groupReadMode, otherReadMode]
  setFdMode fd mode'
  closeFd fd

-- | Generate some text from x and see if it matches the contents of
-- the file at path.  This was originally intended to write files
-- containing template-haskell splices, but now the signature has been
-- generalized.
compareSaveAndReturn :: (FilePath -> IO a) -> (a -> String) -> FilePath -> a -> IO a
compareSaveAndReturn onChange pprint path x =
    do let txt = pprint x
       result <- createFile path txt
       case result of
         Modified -> LL.writeFile (path ++ ".new") txt >> onChange path
         _ -> removeFileIfPresent (path ++ ".new") >> return x

changeError :: FilePath -> IO a
changeError path =
    error (unlines [path ++ " changed, you may need to add Migrate instances for" ++
                    "the updated path types to Appraisal.PathMigrate and check in the new version of " ++ path])
