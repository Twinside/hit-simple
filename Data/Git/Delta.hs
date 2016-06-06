-- |
-- Module      : Data.Git.Delta
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
{-# LANGUAGE BangPatterns #-}
module Data.Git.Delta
    ( Delta(..)
    , DeltaCmd(..)
    , deltaParse
    , deltaRead
    , deltaApply
    ) where

import Control.Applicative( (<$>), (<*>), many )
import Data.Attoparsec
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Lazy as AL
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Builder as BB
import Data.Bits
import Data.Word

-- | a delta is a source size, a destination size and a list of delta cmd
data Delta = Delta 
  { deltaSourceSize :: !Word64
  , deltaDestSize   :: !Word64
  , deltaCommands   :: ![DeltaCmd]
  }
  deriving (Show,Eq)

-- | possible commands in a delta
data DeltaCmd
  = DeltaCopy !ByteString     -- command to insert this bytestring
  | DeltaSrc  !Word64 !Word64 -- command to copy from source (offset, size)
  deriving (Show,Eq)

-- | parse a delta.
-- format is 2 variable sizes, followed by delta cmds. for each cmd:
-- * if first byte MSB is set, we copy from source.
-- * otherwise, we copy from delta.
-- * extensions are not handled.
deltaParse :: A.Parser Delta
deltaParse = do
    srcSize <- getDeltaHdrSize
    resSize <- getDeltaHdrSize
    dcmds   <- many (anyWord8 >>= parseWithCmd)
    return $ Delta srcSize resSize dcmds
  where
    getDeltaHdrSize = go 0 0 where
      go !shift !acc = do
        !v <- anyWord8
        let !acc' = acc .|. (fromIntegral v `clearBit` 7) `unsafeShiftL` shift
        if v `testBit` 7
          then go (shift + 7) acc' 
          else return acc'
 
    -- parse one command, either an extension, a copy from src, or a copy from delta.
    parseWithCmd 0 = error "delta extension not supported"
    parseWithCmd cmd | cmd `testBit` 7 = do
      !o1 <- word8cond (cmd `testBit` 0) 0
      !o2 <- word8cond (cmd `testBit` 1) 8
      !o3 <- word8cond (cmd `testBit` 2) 16 
      !o4 <- word8cond (cmd `testBit` 3) 24
      !s1 <- word8cond (cmd `testBit` 4) 0
      !s2 <- word8cond (cmd `testBit` 5) 8
      !s3 <- word8cond (cmd `testBit` 6) 16 
      let !offset = o1 .|. o2 .|. o3 .|. o4
          !size   = s1 .|. s2 .|. s3
      return $! DeltaSrc offset (if size == 0 then 0x10000 else size)
    parseWithCmd cmd = DeltaCopy <$> A.take (fromIntegral cmd)
 
    word8cond False _ = return 0
    word8cond True sh =
      (\w  -> fromIntegral w `unsafeShiftL` sh) <$> anyWord8

-- | read one delta from a lazy bytestring.
deltaRead :: L.ByteString -> Maybe Delta
deltaRead = AL.maybeResult . AL.parse deltaParse

-- | apply a delta on a lazy bytestring, returning a new bytestring.
deltaApply :: L.ByteString -> Delta -> L.ByteString
deltaApply src (Delta srcSize _ deltaCmds)
  | L.length src /= fromIntegral srcSize = error "source size do not match"
deltaApply src (Delta srcSize _ deltaCmds) = BB.toLazyByteString $ foldMap go deltaCmds where
  takeAt sz at = L.take sz . L.drop at

  go (DeltaSrc o s) = 
    BB.lazyByteString $ takeAt (fromIntegral s) (fromIntegral o) src
  go (DeltaCopy b) = BB.byteString b

