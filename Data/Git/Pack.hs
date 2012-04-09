-- |
-- Module      : Data.Git.Pack
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Pack
	( PackedObjectInfo(..)
	, PackedObjectRaw
	-- * Enumerators of packs
	, packEnumerate
	-- * Helpers to process packs
	, packOpen
	, packClose
	-- * Command for the content of a pack
	, packReadHeader
	, packReadMapAtOffset
	, packReadAtOffset
	, packReadRawAtOffset
	, packEnumerateObjects
	-- * turn a packed object into a 
	, packedObjectToObject
	, packObjectFromRaw
	) where

import Control.Applicative ((<$>))
import Control.Arrow (second)
import Control.Monad

import System.IO
import System.FilePath
import System.Directory

import Data.Bits
import Data.List
import qualified Data.ByteString.Lazy as L

import Data.Attoparsec (anyWord8)
import qualified Data.Attoparsec as A
import qualified Data.Attoparsec.Lazy as AL

import Data.Git.Internal
import Data.Git.Path
import Data.Git.Object
import Data.Git.Delta
import Data.Git.Ref
import Data.Git.FileReader

import Data.Word

type PackedObjectRaw = (PackedObjectInfo, L.ByteString)

data PackedObjectInfo = PackedObjectInfo
	{ poiType       :: ObjectType
	, poiOffset     :: Word64
	, poiSize       :: Word64
	, poiActualSize :: Word64
	, poiExtra      :: Maybe ObjectPtr
	} deriving (Show,Eq)

-- | Enumerate the pack refs available in this repository.
packEnumerate :: FilePath -> IO [Ref]
packEnumerate repoPath = map onlyHash . filter isPackFile <$> getDirectoryContents (repoPath </> "objects" </> "pack")
	where
		isPackFile x = ".pack" `isSuffixOf` x
		onlyHash = fromHexString . takebut 5 . drop 5
		takebut n l = take (length l - n) l

-- | open a pack
packOpen :: FilePath -> Ref -> IO FileReader
packOpen repoPath packRef = openFile (packPath repoPath packRef) ReadMode >>= fileReaderNew False

-- | close a pack
packClose :: FileReader -> IO ()
packClose = fileReaderClose

-- | return the number of entries in this pack
packReadHeader :: FilePath -> Ref -> IO Word32
packReadHeader repoPath packRef =
	withFileReader (packPath repoPath packRef) $ \filereader ->
		fileReaderParse filereader parseHeader
	where parseHeader = do
		packMagic <- be32 <$> A.take 4
		when (packMagic /= 0x5041434b) $ error "not a git packfile"
		ver <- be32 <$> A.take 4
		when (ver /= 2) $ error ("pack file version not supported: " ++ show ver)
		be32 <$> A.take 4

-- | read an object at a specific position using a map function on the objectData
packReadMapAtOffset :: FileReader -> Word64 -> (L.ByteString -> L.ByteString)
                    -> IO (Maybe GitObject)
packReadMapAtOffset fr offset mapData = fileReaderSeek fr offset >> getNextObject fr mapData

-- | read an object at a specific position
packReadAtOffset :: FileReader -> Word64 -> IO (Maybe GitObject)
packReadAtOffset fr offset = packReadMapAtOffset fr offset id

-- | read a raw representation at a specific position
packReadRawAtOffset :: FileReader -> Word64 -> IO (PackedObjectRaw)
packReadRawAtOffset fr offset = fileReaderSeek fr offset >> getNextObjectRaw fr

-- | enumerate all objects in this pack and callback to f for reach raw objects
packEnumerateObjects :: FilePath -> Ref -> Int -> (PackedObjectRaw -> IO a) -> IO ()
packEnumerateObjects repoPath packRef entries f =
	withFileReader (packPath repoPath packRef) $ \filebuffer -> do
		fileReaderSeek filebuffer 12
		parseNext filebuffer entries
	where
		parseNext :: FileReader -> Int -> IO ()
		parseNext _  0    = return ()
		parseNext fr ents = getNextObjectRaw fr >>= f >> parseNext fr (ents-1)

getNextObject :: FileReader -> (L.ByteString -> L.ByteString) -> IO (Maybe GitObject)
getNextObject fr mapData =
	packedObjectToObject . second mapData <$> getNextObjectRaw fr

packedObjectToObject :: (PackedObjectInfo, L.ByteString) -> Maybe GitObject
packedObjectToObject (PackedObjectInfo { poiType = ty, poiExtra = extra }, objData) =
	packObjectFromRaw (ty, extra, objData)


packObjectFromRaw :: (ObjectType, Maybe ObjectPtr, L.ByteString)
                  -> Maybe GitObject
packObjectFromRaw (TypeCommit, Nothing, objData) =
    AL.maybeResult $ AL.parse commitParse objData
packObjectFromRaw (TypeTree, Nothing, objData)   =
    AL.maybeResult $ AL.parse treeParse objData
packObjectFromRaw (TypeBlob, Nothing, objData)   =
    AL.maybeResult $ AL.parse blobParse objData
packObjectFromRaw (TypeTag, Nothing, objData)    =
    AL.maybeResult $ AL.parse tagParse objData
packObjectFromRaw (TypeDeltaOff, Just (PtrOfs o), objData) =
    DeltaOfs o <$> deltaRead objData
packObjectFromRaw (TypeDeltaRef, Just (PtrRef r), objData) =
    DeltaRef r <$> deltaRead objData
packObjectFromRaw _                              =
    error "can't happen unless someone change getNextObjectRaw"

getNextObjectRaw :: FileReader -> IO PackedObjectRaw
getNextObjectRaw fr = do
	sobj      <- fileReaderGetPos fr
	(ty, size) <- fileReaderParse fr parseObjectHeader
	extra      <- case ty of
		TypeDeltaRef -> Just . PtrRef . fromBinary <$> fileReaderGetBS 20 fr
		TypeDeltaOff -> Just . PtrOfs . deltaOffFromList <$> fileReaderGetVLF fr
		_            -> return Nothing
	objData    <- fileReaderInflateToSize fr size
	eobj       <- fileReaderGetPos fr

	return (PackedObjectInfo ty sobj (eobj - sobj) size extra, objData)
	where
		parseObjectHeader = do
			(m, ty, sz) <- splitFirst <$> anyWord8
			size <- if m then (sz +) <$> getNextSize 4 else return sz
			return (ty, size)
			where
				getNextSize n = do
					(c, sz) <- splitOther n <$> anyWord8
					if c then (sz +) <$> getNextSize (n+7) else return sz

				splitFirst :: Word8 -> (Bool, ObjectType, Word64)
				splitFirst w = (w `testBit` 7, toEnum $ fromIntegral ((w `shiftR` 4) .&. 0x7), fromIntegral (w .&. 0xf))
				splitOther n w = (w `testBit` 7, fromIntegral (w .&. 0x7f) `shiftL` n)

		deltaOffFromList (x:xs) = foldl' acc (fromIntegral (x `clearBit` 7)) xs
			where acc a w = ((a+1) `shiftL` 7) + fromIntegral (w `clearBit` 7)
		deltaOffFromList [] = error "cannot happen"
