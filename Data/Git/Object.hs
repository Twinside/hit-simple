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
	, TreeEntry
	, FileRights
	, CommitAuthor( .. )
	, RefSpec( .. )

    , objectTypeUnmarshall
	, objectTypeIsDelta
	-- * parsing function
	, treeParse
	, commitParse
	, tagParse
	, blobParse
	, packedRefParse
	) where

import Data.Git.Ref
import Data.List( groupBy )
import Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L

import qualified Data.Attoparsec.Lazy as A
import Data.Attoparsec.Lazy
import qualified Data.Attoparsec.Lazy as P
import qualified Data.Attoparsec.Char8 as PC
import Control.Applicative ((<$>), many, (<*>), (<*), (*>), (<|>), pure )
import Control.Monad

import Data.Word

-- | location of an object in the database
data ObjectLocation = NotFound 
                    | Loose Ref
                    | Packed Ref Word64
                    deriving (Show,Eq)

-- | Type defined only to get a meaningfull name in compilation
-- errors and other data types
type FileRights = Int

-- | Represent one entry in the tree
-- (permission,file or directory name, blob or tree ref)
type TreeEntry = (FileRights, ByteString, Ref)

-- | an author or committer line
-- has the format: name <email> time timezone
-- FIXME: should be a string, but I don't know if the data is stored
-- consistantly in one encoding (UTF8)
data CommitAuthor = CommitAuthor
    { authorName :: ByteString
    , authorMail :: ByteString
    , authorTimestamp :: Int
    , authorTimezone  :: Int
    }
    deriving (Eq, Show)

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
data GitObject = Tree       [TreeEntry]  -- ^ Represent a folder with entry for children
               | Blob       L.ByteString -- ^ Represent a content (file)
               | Commit     CommitInfo   -- ^ Represent a commit, with associated informations
               | Tag        TagInfo
               deriving (Eq, Show)

data CommitInfo = CommitInfo
	{ commitTree      :: Ref
	, commitParents   :: [Ref]
	, commitAuthor    :: CommitAuthor
	, commitCommitter :: CommitAuthor
	, commitMessage   :: ByteString
	}
	deriving (Show,Eq)

data TagInfo = TagInfo
	{ tagRef        :: Ref
	, tagObjectType :: ObjectType
	, tagBlob       :: ByteString
	, tagName       :: CommitAuthor
	, tagS          :: ByteString
	}
	deriving (Show,Eq)

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
skipChar = void . PC.char

referenceHex ::  A.Parser Ref
referenceHex = fromHex <$> P.take 40

referenceBin :: Parser Ref
referenceBin = fromBinary <$> P.take 20

data RefSpec = RefLocal Ref String
             | RefOther Ref String
             | RefTag Ref String
             | RefRemote String [(Ref, String)]
             deriving (Eq, Show)

packedRefParse :: A.Parser [RefSpec]
packedRefParse = concat . map flattenInfo . groupBy branchName . concat 
              <$> PC.many1 specParse
  where branchName (RefRemote a _) (RefRemote b _) = a == b
        branchName _ _ = False

        flattenInfo :: [RefSpec] -> [RefSpec]
        flattenInfo [] = []
        flattenInfo [x] = [x]
        flattenInfo lst@(RefRemote remote _:_) =
            [RefRemote remote [(ref, branch) |
                            RefRemote _ ((ref,branch):_) <- lst]]
        flattenInfo a = a

        commentParse = PC.char '#' *> PC.takeTill ((== '\n'))
                                   *> separator
                                   *> pure []
        separator = PC.endOfInput
                 <|> (PC.char '\n' *> pure ())

        space = PC.char '\t' <|> PC.char ' '

        refParse = do
            ref <- referenceHex <* space <* string "refs/"
            let tagParser = RefTag ref
                         <$> (PC.string "tags/"
                                *> (BC.unpack <$> PC.takeWhile (/= '\n'))
                                <* separator)
                         <?> "TAG"

                gitRemote = do
                    remoteName <- PC.takeWhile (\c -> c /= '/' && c/= '\n') <* PC.char '/'
                    branch <- PC.takeWhile (/= '\n')
                    separator
                    pure $ RefRemote (BC.unpack remoteName)
                                     [(ref, BC.unpack branch)]
                gitSvnRemote = do
                    RefOther ref . BC.unpack
                        <$> PC.takeWhile (/= '\n') <* separator

                remoteParser = do
                    _ <- PC.string "remotes/"
                    gitRemote <|> gitSvnRemote

                localParser = do
                    RefLocal ref . BC.unpack
                        <$> (PC.string "heads/"
                                *> PC.takeWhile (/= '\n')
                               <* separator)
                stashParser = do
                    RefLocal ref . BC.unpack
                        <$> (PC.string "stash" <* PC.takeWhile (/= '\n') <* separator)

                bisectParser = do
                    RefLocal ref . BC.unpack
                        <$> (B.append <$> PC.string "bisect/" <*> PC.takeWhile (/= '\n') <* separator)

            (:[]) <$> (tagParser <|> remoteParser <|> localParser <|> bisectParser <|> stashParser)

        specParse = commentParse <|> refParse

-- | parse a tree content
treeParse :: A.Parser GitObject
treeParse = Tree <$> parseEnts
    where parseEnts = atEnd >>= \end -> if end then return []
                                               else liftM2 (:) parseEnt parseEnts
          parseEnt = (,,) <$> octal 
                          <*> (PC.char ' ' *> takeTill (== 0))
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
            objectTypeUnmarshall . BC.unpack <$> (string "type " *> takeTill (== 0x0a))
        tagInfo = TagInfo
               <$> (string "object " *> referenceHex) <* skipChar '\n'
               <*> (objType <* skipChar '\n')
               <*> (string "tag " *> takeTill (== 0x0a)) <* skipChar '\n'
               <*> (string "tagger " *> parseName) <* skipChar '\n'
               <*> takeByteString

parseName :: A.Parser CommitAuthor
parseName = CommitAuthor
         <$> (B.init <$> PC.takeWhile (/= '<')) <* skipChar '<'
         <*> PC.takeWhile (/= '>') <* string "> "
         <*> PC.decimal <* string " "
         <*> PC.signed PC.decimal <* skipChar '\n'

