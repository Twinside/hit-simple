-- |
-- Module      : Data.Git.FileWriter
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.FileWriter where

import Data.Git.Ref
import Data.IORef
import qualified Data.ByteString as B
import Codec.Zlib
import Control.Exception (bracket)

import qualified Crypto.Hash.SHA1 as SHA1

import System.IO

defaultCompression :: Int
defaultCompression = 6

data FileWriter = FileWriter
	{ writerHandle  :: Handle
	, writerDeflate :: Deflate
	, writerDigest  :: IORef SHA1.Ctx
	}

fileWriterNew :: Handle -> IO FileWriter
fileWriterNew handle = do
	deflate <- initDeflate defaultCompression defaultWindowBits
	digest  <- newIORef SHA1.init
	return $ FileWriter
		{ writerHandle  = handle
		, writerDeflate = deflate
		, writerDigest  = digest
		}

withFileWriter :: FilePath -> (FileWriter -> IO a) -> IO a
withFileWriter path f =
	bracket (openFile path WriteMode) (hClose) $ \handle ->
		bracket (fileWriterNew handle) (fileWriterClose) f

postDeflate :: Handle -> Maybe B.ByteString -> IO ()
postDeflate _      Nothing    = return ()
postDeflate handle (Just dbs) = B.hPut handle dbs

-- withDeflateInput :: Deflate -> ByteString -> (IO (Maybe ByteString) -> IO a)
--                  -> IO a
fileWriterOutput :: FileWriter -> B.ByteString -> IO ()
fileWriterOutput (FileWriter { writerHandle = handle, writerDigest = digest, writerDeflate = deflate }) bs = do
	putStrLn ("outputing" ++ show bs)
	modifyIORef digest (\ctx -> SHA1.update ctx bs)
	deflated <- feedDeflate deflate bs 
	deflated >>= postDeflate handle

fileWriterClose :: FileWriter -> IO ()
fileWriterClose (FileWriter { writerHandle = handle, writerDeflate = deflate }) =
	finishDeflate deflate >>= postDeflate handle

fileWriterGetDigest :: FileWriter -> IO Ref
fileWriterGetDigest (FileWriter { writerDigest = digest }) =
    (fromBinary . SHA1.finalize) `fmap` readIORef digest
