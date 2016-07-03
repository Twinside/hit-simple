{-# OPTIONS_GHC -fwarn-missing-signatures #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module Data.Git.Index
    ( IndexEntry( .. )
    , parseIndex
    , decodeIndex
    , loadIndexFile
    , indexEntryOfFile
    ) where

import Prelude hiding ( FilePath, readFile )
import Filesystem
import Filesystem.Path hiding (concat)
import Control.Monad( when
                    , replicateM
                    )
import Data.Bits
    ( unsafeShiftL
    , unsafeShiftR
    , testBit
    , (.&.)
    , (.|.)
    )
import Data.Word( Word8, Word16, Word32 )
import Data.Git.Ref
import qualified Control.Exception as E
import qualified Data.Vector as V
import qualified Data.Attoparsec.ByteString as A
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.Git.Parser as P

data IndexHeader = IndexHeader 
  { _indexFileVersion :: {-# UNPACK #-} !Word32
  , _indexEntryCount  :: {-# UNPACK #-} !Word32
  }
  deriving (Show, Eq)

-- | Finds an index in a given vector, which must be sorted with respect to the
-- given comparison function, at which the given element could be inserted while
-- preserving the vector's sortedness.
binarySearchBy :: (e -> a -> Ordering) -> V.Vector e -> a -> Maybe e
{-# INLINE binarySearchBy #-}
binarySearchBy cmp vec e = go 0 (length vec) where
  go !l !u | u <= l    = Nothing
  go !l !u =
     let !k = (u + l) `unsafeShiftR` 1
         !e' = V.unsafeIndex vec k in
     case cmp e' e of
       LT -> go (k+1) u
       EQ -> Just e'
       GT -> go l     k

indexEntryOfFile :: B.ByteString -> V.Vector IndexEntry -> (Maybe IndexEntry)
indexEntryOfFile path vec = binarySearchBy (compare . _fileName) vec path

loadIndexFile :: FilePath -> IO (Either String (V.Vector IndexEntry))
loadIndexFile path = (decodeIndex <$> readFile path) `E.catch` onError
  where
    onError :: E.SomeException -> IO (Either String a)
    onError _ = return $ Left "Cannot find index file"

decodeIndex :: B.ByteString -> Either String (V.Vector IndexEntry)
decodeIndex = A.parseOnly parseIndex

parseIndex :: A.Parser (V.Vector IndexEntry)
parseIndex = do
  hdr <- parseIndexHeader
  V.replicateM (fromIntegral $ _indexEntryCount hdr) parseIndexEntry

parseBe32 :: A.Parser Word32
parseBe32 = do
  w1 <- parseBe16
  w2 <- parseBe16
  return $ fromIntegral w1 `unsafeShiftL` 16 .|. fromIntegral w2

parseBe16 :: A.Parser Word16
parseBe16 = do
  w1 <- P.anyWord8
  w2 <- P.anyWord8
  return $ fromIntegral w1 `unsafeShiftL` 8 .|. fromIntegral w2

parseRef :: A.Parser Ref
parseRef = fromBinary <$> A.take 20

parseIndexHeader :: A.Parser IndexHeader
parseIndexHeader = do
  magic <- A.take 4
  when (magic /= "DIRC") $
    fail "wrong magic number for index"
  ver <- parseBe32
  when (ver `notElem` [2, 3]) $
    fail "unsupported packIndex version"
  entries <- parseBe32
  return $ IndexHeader ver entries

-- Index entries are sorted in ascending order on the name field,
-- interpreted as a string of unsigned bytes (i.e. memcmp() order, no
-- localization, no special casing of directory separator '/'). Entries
-- with the same name are sorted by their stage field.
data IndexEntry = IndexEntry
  { -- | 32-bit ctime seconds, the last time a file's metadata changed
    _ctime :: !Word32
    -- | 32-bit ctime nanosecond fractions
  , _ctimeNano :: !Word32
    -- | 32-bit mtime seconds, the last time a file's data changed
  , _mtime :: !Word32
    -- | 32-bit mtime nanosecond fractions
  , _mtimeNano :: !Word32
   -- | 32-bit dev
  , _dev :: !Word32
   -- | 32-bit ino
  , _ino :: !Word32
   -- | 32-bit mode, split into (high to low bits)
   -- 
   -- 4-bit object type
   --   valid values in binary are 1000 (regular file), 1010 (symbolic link)
   --   and 1110 (gitlink)
   -- 
   -- 3-bit unused
   -- 
   -- 9-bit unix permission. Only 0755 and 0644 are valid for regular files.
   -- Symbolic links and gitlinks have value 0 in this field.
  , _mode :: !Word32
    -- | 32-bit uid
  , _uid  :: !Word32
    -- | 32-bit gid
  , _gid  :: !Word32
    -- | 32-bit file size This is the on-disk size from stat(2), truncated to 32-bit.
  , _fileSize :: !Word32
    -- | 160-bit SHA-1 for the represented object
  , _hash :: !Ref
   
    -- | A 16-bit 'flags' field
  , _flags :: !IndexEntryFlags
    -- (Version 3 or later) A 16-bit field, only applicable if the
    -- "extended flag" above is 1, split into (high to low bits).
    -- 
    -- 1-bit reserved for future
    -- 
    -- 1-bit skip-worktree flag (used by sparse checkout)
    -- 
    -- 1-bit intent-to-add flag (used by "git add -N")
    -- 
    -- 13-bit unused, must be zero
  , _extended :: !Word16

  , _fileName :: !B.ByteString
  }
  deriving (Eq, Show)

data IndexEntryFlags = IndexEntryFlags 
  { -- | 1-bit assume-valid flag
    _iefAssumeValid :: !Bool
    -- | 1-bit extended flag (must be zero in version 2)
  , _iefExtended    :: !Bool
    -- | 2-bit stage (during merge)
  , _iefStage       :: {-# UNPACK #-} !Word8
    -- | 12-bit name length if the length is less than 0xFFF;
    -- otherwise 0xFFF is stored in this field.
  , _iefNameLength  :: {-# UNPACK #-} !Word16
  }
  deriving (Eq, Show)

flagsOfWord :: Word16 -> IndexEntryFlags
flagsOfWord w = IndexEntryFlags
  { _iefAssumeValid = w `testBit` 15
  , _iefExtended    = w `testBit` 14
  , _iefStage       = fromIntegral $ (w `unsafeShiftR` 13) .&. 0x3
  , _iefNameLength  = w .&. 0xFFF
  }

parseIndexEntry :: A.Parser IndexEntry
parseIndexEntry = do
    ctime <- parseBe32      -- +4   4
    ctimeNano <- parseBe32  -- +4   8
    mtime <- parseBe32      -- +4   12
    mtimeNano <- parseBe32  -- +4   16
    dev   <- parseBe32      -- +4   20
    inode <- parseBe32-- +4   24
    mode  <- parseBe32-- +4   28
    uid   <- parseBe32-- +4   32
    gid   <- parseBe32-- +4   36
    size  <- parseBe32-- +4   40
    fileHash  <- parseRef-- +20   60
    flags <- flagsOfWord <$> parseBe16 -- +2 62
    extended <- if _iefExtended flags -- +2 64
        then parseBe16
        else return 0
    name  <- A.take . fromIntegral $ _iefNameLength flags
    let padding = 8 - ((62 + _iefNameLength flags) `mod` 8)
    _ <- replicateM (fromIntegral padding) A.anyWord8
    return IndexEntry
        { _ctime = ctime
        , _ctimeNano = ctimeNano
        , _mtime = mtime
        , _mtimeNano = mtimeNano
        , _dev = dev
        , _ino = inode
        , _mode = mode
        , _uid  = uid
        , _gid  = gid
        , _fileSize = size
        , _hash = fileHash
        , _flags = flags
        , _extended = extended
        , _fileName = name
        }


{-

<INDEX_CONTENTS_EXTENSIONS>
    :   ( <INDEX_EXTENSION> )*
    ;
-}

parseIndexExtension :: A.Parser (BC.ByteString, BC.ByteString)
parseIndexExtension = do
    -- # 4 byte sequence identifying how the <INDEX_EXTENSION_DATA>
    -- # should be interpreted. If the first byte has a value greater
    -- # than or equal to the ASCII character 'A' (0x41) and less than
    -- # or equal to the ASCII character 'Z' (0x5a), the extension is
    -- # optional and does not affect the interpretation of the other
    -- # contents in the index file. Any non-optional extensions must
    -- # be understood by the reading application to correctly
    -- # interpret the index file contents.
    name     <- A.take 4
    dataSize <- parseBe32
    data_    <- A.take $ fromIntegral dataSize
    return (name,data_)

