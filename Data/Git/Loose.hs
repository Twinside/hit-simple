{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Data.Git.Loose
    (
    -- * marshall from and to lazy bytestring
      looseUnmarshall
    , looseUnmarshallRaw

    -- * read and check object existence
    , looseRead
    , looseReadHeader
    , looseReadRaw
    , looseExists

    -- * enumeration of loose objects
    , looseEnumerateWithPrefixFilter
    ) where

import Codec.Compression.Zlib

import System.FilePath
import System.Directory

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import Data.Word( Word64 )
import Data.Attoparsec.Lazy
import qualified Data.Attoparsec.Char8 as PC
import Control.Applicative ((<$>), (<|>), (<*))
import Control.Exception (SomeException)
import qualified Control.Exception as E

import Data.Git.Ref
import Data.Git.Object
import Data.Git.Path

decimal :: Parser Int
decimal = PC.decimal

-- loose object parsing
parseHeader :: Parser (ObjectType, Word64, Maybe a)
parseHeader = do
    h <- takeWhile1 ((/=) 0x20) <* word8 0x20
    sz <- decimal
    return (objectTypeUnmarshall $ BC.unpack h, fromIntegral sz, Nothing)


parseObject :: L.ByteString -> GitObject
parseObject = parseSuccess (parseTree <|> parseBlob <|> parseCommit <|> parseTag)
    where parseSuccess p = either error id . eitherResult . parse p
          parseTreeHeader   = string "tree " >> decimal >> word8 0
          parseTagHeader    = string "tag " >> decimal >> word8 0
          parseCommitHeader = string "commit " >> decimal >> word8 0
          parseBlobHeader   = string "blob " >> decimal >> word8 0

          parseTree   = parseTreeHeader >> treeParse
          parseTag    = parseTagHeader >> tagParse
          parseCommit = parseCommitHeader >> commitParse
          parseBlob   = parseBlobHeader >> blobParse

-- | unmarshall an object (with header) from a lazy bytestring.
looseUnmarshall :: L.ByteString -> GitObject
looseUnmarshall = parseObject . decompress

-- | unmarshall an object as (header, data) tuple from a lazy bytestring.
looseUnmarshallRaw :: L.ByteString -> (ObjectHeader, ObjectData)
looseUnmarshallRaw l =
    let dl = decompress l in
    let i = L.findIndex ((==) 0) dl in
    case i of
        Nothing  -> error "object not right format. missing 0"
        Just idx ->
            let (h, r) = L.splitAt (idx+1) dl in
            case maybeResult $ parse parseHeader h of
                Nothing  -> error "cannot open object"
                Just hdr -> (hdr, r)

-- | read a specific ref from a loose object and returns an header and data.
looseReadRaw :: FilePath -> Ref -> IO (ObjectHeader, ObjectData)
looseReadRaw repoPath ref = looseUnmarshallRaw <$> L.readFile (objectPathOfRef repoPath ref)

-- | read only the header of a loose object.
looseReadHeader :: FilePath -> Ref -> IO (ObjectType, Word64, Maybe a)
looseReadHeader repoPath ref = toHeader <$> L.readFile (objectPathOfRef repoPath ref)
    where toHeader = either error id . eitherResult . parse parseHeader . decompress

-- | read a specific ref from a loose object and returns an object
looseRead :: FilePath -> Ref -> IO GitObject
looseRead repoPath ref = looseUnmarshall <$> L.readFile (objectPathOfRef repoPath ref)

-- | check if a specific ref exists as loose object
looseExists :: FilePath -> Ref -> IO Bool
looseExists repoPath ref = doesFileExist (objectPathOfRef repoPath ref)

-- | enumerate all references available with a specific prefix.
looseEnumerateWithPrefixFilter :: FilePath -> String -> (Ref -> Bool) -> IO [Ref]
looseEnumerateWithPrefixFilter repoPath prefix filterF =
    filter filterF . map (fromHexString . (prefix ++)) . filter isRef <$> getDir (repoPath </> "objects" </> prefix)
    where
        getDir p = E.catch (getDirectoryContents p) (\(_::SomeException) -> return [])
        isRef l = length l == 38

