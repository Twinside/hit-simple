{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE OverloadedStrings #-}
-- | Definition of different git objects
module Data.Git.Object
	( ObjectLocation(..)
	, ObjectType(..)
	, ObjectHeader
	, ObjectData
	, ObjectPtr(..)
	, GitObject(..)
	, CommitInfo(..)
	, TagInfo(..)
	, ObjectInfo(..)
	, TreeEnt
	, objectTypeMarshall
	, objectTypeUnmarshall
	, objectToType 
	, objectTypeIsDelta
	, objectIsDelta
	-- * parsing function
	, treeParse
	, commitParse
	, tagParse
	, blobParse
	-- * writing function
	, objectWriteHeader
	, objectWrite
	, objectHash
	) where

import Data.Git.Ref
import Data.Git.Delta

import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import qualified Data.Attoparsec.Lazy as A
import Data.Attoparsec.Lazy
import qualified Data.Attoparsec.Lazy as P
import qualified Data.Attoparsec.Char8 as PC
import Control.Applicative ((<$>), many, (<*>), (<*), (*>) )
import Control.Monad

import Data.Word
import Text.Printf

-- | location of an object in the database
data ObjectLocation = NotFound 
                    | Loose Ref
                    | Packed Ref Word64
                    deriving (Show,Eq)

-- | represent one entry in the tree
-- (permission,file or directory name,blob or tree ref)
-- name should maybe a filepath, but not sure about the encoding.
type TreeEnt = (Int,ByteString,Ref)

-- | an author or committer line
-- has the format: name <email> time timezone
-- FIXME: should be a string, but I don't know if the data is stored
-- consistantly in one encoding (UTF8)
type Name = (ByteString,ByteString,Int,Int)

-- | type of a git object.
data ObjectType =
	  TypeTree
	| TypeBlob
	| TypeCommit
	| TypeTag
	| TypeDeltaOff
	| TypeDeltaRef
	deriving (Show,Eq)

-- | Delta objects points to some others objects in the database
-- either as offset in the pack or as a direct reference.
data ObjectPtr = PtrRef Ref
               | PtrOfs Word64
               deriving (Show,Eq)

type ObjectHeader = (ObjectType, Word64, Maybe ObjectPtr)

type ObjectData = L.ByteString

-- | Raw objects infos have an header (type, size, ptr),
-- the data and a pointers chains to parents for resolved objects.
data ObjectInfo = ObjectInfo
	{ oiHeader :: ObjectHeader
	, oiData   :: ObjectData
	, oiChains :: [ObjectPtr]
	}
	deriving (Show,Eq)

-- | describe a git object, that could of 6 differents types:
-- tree, blob, commit, tag and deltas (offset or ref).
-- the deltas one are only available through packs.
data GitObject = Tree { treeGetEnts :: [TreeEnt] }
               | Blob { blobGetContent :: L.ByteString }
               | Commit CommitInfo
               | Tag    TagInfo
               | DeltaOfs Word64 Delta
               | DeltaRef Ref Delta
               deriving (Eq, Show)

objectToType :: GitObject -> ObjectType
objectToType (Tree _) = TypeTree
objectToType (Blob _) = TypeBlob
objectToType (Commit _) = TypeCommit
objectToType (Tag _) = TypeTag
objectToType (DeltaOfs _ _) = TypeDeltaOff
objectToType (DeltaRef _ _) = TypeDeltaRef

data CommitInfo = CommitInfo
	{ commitTreeish   :: Ref
	, commitParents   :: [Ref]
	, commitAuthor    :: Name
	, commitCommitter :: Name
	, commitMessage   :: ByteString
	}
	deriving (Show,Eq)

data TagInfo = TagInfo
	{ tagRef        :: Ref
	, tagObjectType :: ObjectType
	, tagBlob       :: ByteString
	, tagName       :: Name
	, tagS          :: ByteString
	}
	deriving (Show,Eq)

objectTypeMarshall :: ObjectType -> String
objectTypeMarshall TypeTree   = "tree"
objectTypeMarshall TypeBlob   = "blob"
objectTypeMarshall TypeCommit = "commit"
objectTypeMarshall TypeTag    = "tag"
objectTypeMarshall _          = error "deltas cannot be marshalled"

objectTypeUnmarshall :: String -> ObjectType
objectTypeUnmarshall "tree"   = TypeTree
objectTypeUnmarshall "blob"   = TypeBlob
objectTypeUnmarshall "commit" = TypeCommit
objectTypeUnmarshall "tag"    = TypeTag
objectTypeUnmarshall _        = error "unknown object type"

objectTypeIsDelta :: ObjectType -> Bool
objectTypeIsDelta TypeDeltaOff = True
objectTypeIsDelta TypeDeltaRef = True
objectTypeIsDelta _            = False

objectIsDelta :: GitObject -> Bool
objectIsDelta (DeltaOfs _ _) = True
objectIsDelta (DeltaRef _ _) = True
objectIsDelta _ = False

-- | the enum instance is useful when marshalling to pack file.
instance Enum ObjectType where
	fromEnum TypeCommit   = 0x1
	fromEnum TypeTree     = 0x2
	fromEnum TypeBlob     = 0x3
	fromEnum TypeTag      = 0x4
	fromEnum TypeDeltaOff = 0x6
	fromEnum TypeDeltaRef = 0x7

	toEnum 0x1 = TypeCommit
	toEnum 0x2 = TypeTree
	toEnum 0x3 = TypeBlob
	toEnum 0x4 = TypeTag
	toEnum 0x6 = TypeDeltaOff
	toEnum 0x7 = TypeDeltaRef
	toEnum n   = error ("not a valid object: " ++ show n)

octal :: Parser Int
octal = B.foldl' step 0 `fmap` takeWhile1 isOct where
	isOct w = w >= 0x30 && w <= 0x37
	step a w = a * 8 + fromIntegral (w - 0x30)

skipChar :: Char -> Parser ()
skipChar c = PC.char c >> return ()

referenceHex ::  A.Parser Ref
referenceHex = fromHex <$> P.take 40

referenceBin :: Parser Ref
referenceBin = fromBinary <$> P.take 20

-- | parse a tree content
treeParse :: A.Parser GitObject
treeParse = Tree <$> parseEnts
    where parseEnts = atEnd >>= \end -> if end then return []
                                               else liftM2 (:) parseEnt parseEnts
          parseEnt = (,,) <$> octal 
                          <*> (PC.char ' ' *> takeTill ((==) 0))
                          <*> (word8 0 *> referenceBin)

-- | parse a blob content
blobParse :: A.Parser GitObject
blobParse = Blob <$> takeLazyByteString

-- | parse a commit content
commitParse :: A.Parser GitObject
commitParse = Commit <$> commit
   where commit = CommitInfo
               <$> (string "tree " *> referenceHex)
               <*> (skipChar '\n' *> many parseParentRef)
               <*> (string "author " >> parseName)
               <*> (string "committer " *> parseName)
               <*> (skipChar '\n' *> takeByteString)

         parseParentRef =
            (string "parent " *> referenceHex) <* skipChar '\n'
		
-- | parse a tag content
tagParse :: A.Parser GitObject
tagParse = Tag <$> tagInfo
  where objType =
            objectTypeUnmarshall . BC.unpack <$> (string "type " *> takeTill ((==) 0x0a))
        tagInfo = TagInfo
               <$> (string "object " *> referenceHex) <* skipChar '\n'
               <*> (objType <* skipChar '\n')
               <*> (string "tag " *> takeTill ((==) 0x0a)) <* skipChar '\n'
               <*> (string "tagger " *> parseName) <* skipChar '\n'
               <*> takeByteString

parseName :: A.Parser (ByteString, ByteString, Int, Int)
parseName = (,,,)
         <$> (B.init <$> PC.takeWhile ((/=) '<')) <* skipChar '<'
         <*> PC.takeWhile ((/=) '>') <* string "> "
         <*> PC.decimal <* string " "
         <*> PC.signed PC.decimal <* skipChar '\n'

-- header of loose objects, but also useful for any object to determine object's hash
objectWriteHeader :: ObjectType -> Word64 -> ByteString
objectWriteHeader ty sz = BC.pack (objectTypeMarshall ty ++ " " ++ show sz ++ [ '\0' ])

objectWrite :: GitObject-> L.ByteString
objectWrite (Blob bData) = bData
objectWrite (Tree ents) = L.fromChunks . concat $ map writeTreeEnt ents
  where writeTreeEnt (perm,name,ref) =
                [ BC.pack $ printf "%o" perm
                , BC.singleton ' '
                , name
                , B.singleton 0
                , toBinary ref
                ]

objectWrite (Commit (CommitInfo tree parents author committer msg)) =
	L.fromChunks [BC.unlines ls, B.singleton 0xa, msg]
	where
		ls = [ "tree " `BC.append` (toHex tree) ] ++
		     map (BC.append "parent " . toHex) parents ++
		     [ writeName "author" author
		     , writeName "committer" committer ]
objectWrite (Tag (TagInfo ref ty tag tagger signature)) =
	L.fromChunks [BC.unlines ls, B.singleton 0xa, signature]
	where
		ls = [ "object " `BC.append` (toHex ref)
		     , "type " `BC.append` (BC.pack $ objectTypeMarshall ty)
		     , "tag " `BC.append` tag
		     , writeName "tagger" tagger ]

objectWrite _ = error "Cannot marshal delta"


objectHash :: ObjectType -> Word64 -> L.ByteString -> Ref
objectHash ty w lbs = hashLBS $ L.fromChunks (objectWriteHeader ty w : L.toChunks lbs)

-- used for objectWrite for commit and tag
writeName :: ByteString -> (ByteString, ByteString, Int, Int)
          -> ByteString
writeName label (name, email, time, tz) =
	B.concat [label, " ", name, " <", email, "> ", BC.pack (show time), " ", BC.pack (showtz tz)]
	where showtz i | i > 0 = "+" ++ show i
	               | otherwise = show i

