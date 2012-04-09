-- |
-- Module      : Data.Git.Delta
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
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
import Data.Bits
import Data.Word

-- | a delta is a source size, a destination size and a list of delta cmd
data Delta = Delta Word64 Word64 [DeltaCmd]
	deriving (Show,Eq)

-- | possible commands in a delta
data DeltaCmd =
	  DeltaCopy ByteString   -- command to insert this bytestring
	| DeltaSrc Word64 Word64 -- command to copy from source (offset, size)
	deriving (Show,Eq)

-- | parse a delta.
-- format is 2 variable sizes, followed by delta cmds. for each cmd:
-- * if first byte MSB is set, we copy from source.
-- * otherwise, we copy from delta.
-- * extensions are not handled.
deltaParse :: A.Parser Delta
deltaParse = Delta <$> getDeltaHdrSize <*> getDeltaHdrSize
                   <*> many (anyWord8 >>= parseWithCmd)
	where
		getDeltaHdrSize = do
			z <- A.takeWhile (\w -> w `testBit` 7)
			l <- anyWord8
			return $ unbytes 0 $ (map (\w -> w `clearBit` 7) (B.unpack z) ++ [l])
		-- use a foldl ..
		unbytes _  []     = 0
		unbytes sh (x:xs) = (fromIntegral x) `shiftL` sh + unbytes (sh+7) xs
		-- parse one command, either an extension, a copy from src, or a copy from delta.
		parseWithCmd cmd
			| cmd == 0        = error "delta extension not supported"
			| cmd `testBit` 7 = do
				o1 <- word8cond (cmd `testBit` 0) 0
				o2 <- word8cond (cmd `testBit` 1) 8
				o3 <- word8cond (cmd `testBit` 2) 16 
				o4 <- word8cond (cmd `testBit` 3) 24
				s1 <- word8cond (cmd `testBit` 4) 0
				s2 <- word8cond (cmd `testBit` 5) 8
				s3 <- word8cond (cmd `testBit` 6) 16 
				let offset = o1 .|. o2 .|. o3 .|. o4
				let size   = s1 .|. s2 .|. s3
				return $ DeltaSrc offset (if size == 0 then 0x10000 else size)
			| otherwise       = DeltaCopy <$> A.take (fromIntegral cmd)
		word8cond cond sh = do
			if cond then (flip shiftL sh . fromIntegral) <$> anyWord8 else return 0

-- | read one delta from a lazy bytestring.
deltaRead :: L.ByteString -> Maybe Delta
deltaRead = AL.maybeResult . AL.parse deltaParse

-- | apply a delta on a lazy bytestring, returning a new bytestring.
deltaApply :: L.ByteString -> Delta -> L.ByteString
deltaApply src (Delta srcSize _ deltaCmds)
	| L.length src /= fromIntegral srcSize = error "source size do not match"
	| otherwise                            = -- FIXME use a bytestring builder here.
		L.fromChunks $ concatMap resolve deltaCmds where
			resolve (DeltaSrc o s) = L.toChunks $ takeAt (fromIntegral s) (fromIntegral o) src
			resolve (DeltaCopy b)  = [b]
			takeAt sz at = L.take sz . L.drop at
