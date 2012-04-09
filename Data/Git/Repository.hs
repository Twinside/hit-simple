{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
-- |
-- Module      : Data.Git.Repository
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Repository
    ( Git
    , HTree
    , HTreeEnt(..)
    , gitRepoPath
    , openRepo
    , closeRepo
    , withRepo
    , findRepository
    , findReference
    , findReferencesWithPrefix
    , findObjectRaw
    , findObjectRawAt
    , findObject
    , findCommit
    , findTree
    , findObjectAt
    , buildHTree
    , resolvePath
    , resolveTreeish
    , resolveRevision
    , initRepo
    , isRepo
    ) where

import System.Directory
import System.FilePath
import System.Environment

import Control.Applicative ((<$>))
import Control.Exception
import qualified Control.Exception as E
import Control.Monad

import Data.Word
import Data.IORef
import Data.List ((\\), find, isPrefixOf)

import Data.ByteString (ByteString)

import Data.Git.Delta
import Data.Git.FileReader
import Data.Git.Index
import Data.Git.Pack
import Data.Git.Named
import Data.Git.Object
import Data.Git.Revision
import Data.Git.Loose
import Data.Git.Ref

data IndexReader = IndexReader IndexHeader FileReader

-- | represent an git repo, with possibly already opened filereaders
-- for indexes and packs
data Git = Git
    { gitRepoPath  :: FilePath
    , indexReaders :: IORef [(Ref, IndexReader)]
    , packReaders  :: IORef [(Ref, FileReader)]
    }

-- | hierarchy tree, either a reference to a blob (file) or a tree (directory).
data HTreeEnt = TreeDir Ref HTree | TreeFile Ref
type HTree = [(Int,ByteString,HTreeEnt)]

-- | open a new git repository context
openRepo :: FilePath -> IO Git
openRepo path = liftM2 (Git path) (newIORef []) (newIORef [])

-- | close a git repository context, closing all remaining fileReaders.
closeRepo :: Git -> IO ()
closeRepo (Git { indexReaders = ireaders, packReaders = preaders }) = do
    mapM_ (closeIndexReader . snd) =<< readIORef ireaders
    mapM_ (fileReaderClose . snd) =<< readIORef preaders
    where closeIndexReader (IndexReader _ fr) = fileReaderClose fr

-- | find the git repository from the current directory.
findRepository :: IO FilePath
findRepository = do
    menvDir <- E.catch (Just <$> getEnv "GIT_DIR") (\(_:: SomeException) -> return Nothing)
    case menvDir of
        Nothing     -> checkDir 0
        Just envDir -> do
            e <- isRepo envDir
            when (not e) $ error "environment GIT_DIR is not a git repository" 
            return envDir
    where
        checkDir 128 = error "not a git repository"
        checkDir n   = do
            let filepath = concat (replicate n ("../") ++ [".git"])
            e <- isRepo filepath
            if e then return filepath else checkDir (n+1)

-- | execute a function f with a git context.
withRepo :: FilePath -> (Git -> IO c) -> IO c
withRepo path f = bracket (openRepo path) closeRepo f

iterateIndexes :: Git -> (a -> (Ref, IndexReader) -> IO (a,Bool)) -> a -> IO a
iterateIndexes git f initAcc = do
    allIndexes    <- indexEnumerate (gitRepoPath git)
    readers       <- readIORef (indexReaders git)
    (a,terminate) <- loop initAcc readers
    if terminate
        then return a
        else readRemainingIndexes a (allIndexes \\ map fst readers)
    where
        loop acc []     = return (acc, False)
        loop acc (r:rs) = do
            (nacc, terminate) <- f acc r
            if terminate
                then return (nacc,True)
                else loop nacc rs

        readRemainingIndexes acc []            = return acc
        readRemainingIndexes acc (idxref:idxs) = do
            fr <- indexOpen (gitRepoPath git) idxref
            idx <- indexReadHeader fr
            let idxreader = IndexReader idx fr
            let r = (idxref, idxreader)
            modifyIORef (indexReaders git) (\l -> r : l)
            (nacc, terminate) <- f acc r
            if terminate
                then return nacc
                else readRemainingIndexes nacc idxs

findReference :: Git -> Ref -> IO ObjectLocation
findReference git ref = maybe NotFound id <$> (findLoose `mplusIO` findInIndexes)
    where
        findLoose :: IO (Maybe ObjectLocation)
        findLoose = do
            isLoose <- looseExists (gitRepoPath git) ref
            if isLoose then return (Just $ Loose ref) else return Nothing

        findInIndexes :: IO (Maybe ObjectLocation)
        findInIndexes = iterateIndexes git isinIndex Nothing --f -> (a -> IndexReader -> IO (a,Bool)) -> a -> IO a

        isinIndex acc (idxref, (IndexReader idxhdr indexreader)) = do
            mloc <- indexGetReferenceLocation idxhdr indexreader ref
            case mloc of
                Nothing  -> return (acc, False)
                Just loc -> return (Just $ Packed idxref loc, True)

        mplusIO :: IO (Maybe a) -> IO (Maybe a) -> IO (Maybe a)
        mplusIO f g = f >>= \vopt -> case vopt of
            Nothing -> g
            Just v  -> return $ Just v

-- | get all the references that start by a specific prefix
findReferencesWithPrefix :: Git -> String -> IO [Ref]
findReferencesWithPrefix git pre
    | invalidLength         = error "not a valid prefix"
    | not (isHexString pre) = error "reference prefix contains non hexchar"
    | otherwise             = do
        looseRefs  <- looseEnumerateWithPrefixFilter (gitRepoPath git) (take 2 pre) matchRef
        packedRefs <- concat <$> iterateIndexes git idxPrefixMatch []
        return (looseRefs ++ packedRefs)
    where
        -- not very efficient way to do that... will do for now.
        matchRef ref = pre `isPrefixOf` toHexString ref
        invalidLength = length pre < 2 || length pre > 39 

        idxPrefixMatch acc (_, (IndexReader idxhdr indexreader)) = do
            refs <- indexGetReferencesWithPrefix idxhdr indexreader pre
            return (refs:acc,False)

readRawFromPack :: Git -> Ref -> Word64 -> IO (FileReader, PackedObjectRaw)
readRawFromPack git pref offset = do
    readers <- readIORef (packReaders git)
    reader <- case lookup pref readers of
        Just r  -> return r
        Nothing -> do
            p <- packOpen (gitRepoPath git) pref
            modifyIORef (packReaders git) ((pref, p):)
            return p
    po <- packReadRawAtOffset reader offset
    return (reader, po)

readFromPack :: Git -> Ref -> Word64 -> Bool -> IO (Maybe ObjectInfo)
readFromPack git pref o resolveDelta = do
    (reader, x) <- readRawFromPack git pref o
    if resolveDelta then resolve reader o x else return $ Just $ generifyHeader x
    where
        generifyHeader :: PackedObjectRaw -> ObjectInfo
        generifyHeader (po, objData) = ObjectInfo { oiHeader = hdr, oiData = objData, oiChains = [] }
            where hdr = (poiType po, poiActualSize po, poiExtra po)

        resolve :: FileReader -> Word64 -> PackedObjectRaw -> IO (Maybe ObjectInfo)
        resolve reader offset (po, objData) = do
            case (poiType po, poiExtra po) of
                (TypeDeltaOff, Just ptr@(PtrOfs doff)) -> do
                    let delta = deltaRead objData
                    let noffset = offset - doff
                    base <- resolve reader noffset =<< packReadRawAtOffset reader noffset
                    return $ addToChain ptr $ applyDelta delta base
                (TypeDeltaRef, Just ptr@(PtrRef bref)) -> do
                    let delta = deltaRead objData
                    base <- findObjectRaw git bref True
                    return $ addToChain ptr $ applyDelta delta base
                _                                    -> return $ Just $ generifyHeader (po, objData)

        addToChain ptr (Just oi) = Just (oi { oiChains = ptr : oiChains oi })
        addToChain _   Nothing   = Nothing

        applyDelta :: Maybe Delta -> Maybe ObjectInfo -> Maybe ObjectInfo
        applyDelta (Just delta@(Delta _ rSize _)) (Just objInfo) = Just $ objInfo
            { oiHeader = (\(a,_,c) -> (a,rSize,c)) $ oiHeader objInfo
            , oiData   = deltaApply (oiData objInfo) delta
            }
        applyDelta _ _                                      = Nothing


-- | get an object from repository
findObjectRawAt :: Git -> ObjectLocation -> Bool -> IO (Maybe ObjectInfo)
findObjectRawAt _   NotFound    _ = return Nothing
findObjectRawAt git (Loose ref) _ = Just . (\(h,d)-> ObjectInfo h d[]) <$> looseReadRaw (gitRepoPath git) ref
findObjectRawAt git (Packed pref o) resolveDelta = readFromPack git pref o resolveDelta

-- | get an object from repository
findObjectRaw :: Git -> Ref -> Bool -> IO (Maybe ObjectInfo)
findObjectRaw git ref resolveDelta = do
    loc <- findReference git ref
    findObjectRawAt git loc resolveDelta

-- | get an object type from repository
findObjectType :: Git -> Ref -> IO (Maybe ObjectType)
findObjectType git ref = findReference git ref >>= findObjectTypeAt
    where
        findObjectTypeAt NotFound        = return Nothing
        findObjectTypeAt (Loose _)       = Just . (\(t,_,_) -> t) <$> looseReadHeader (gitRepoPath git) ref
        findObjectTypeAt (Packed pref o) =
            fmap ((\(ty,_,_) -> ty) . oiHeader) <$> readFromPack git pref o True

-- | get an object from repository using a location to reference it.
findObjectAt :: Git -> ObjectLocation -> Bool
             -> IO (Maybe GitObject)
findObjectAt git loc resolveDelta = 
  maybe Nothing toObject <$> findObjectRawAt git loc resolveDelta
    where toObject (ObjectInfo { oiHeader = (ty, _, extra), oiData = objData }) =
                packObjectFromRaw (ty, extra, objData)

-- | get an object from repository using a ref.
findObject :: Git -> Ref -> Bool -> IO (Maybe GitObject)
findObject git ref resolveDelta = maybe Nothing toObject <$> findObjectRaw git ref resolveDelta
    where
        toObject (ObjectInfo { oiHeader = (ty, _, extra), oiData = objData }) = packObjectFromRaw (ty, extra, objData)

-- should be a standard function that do that...
mapJustM :: Monad m => (t -> m (Maybe a)) -> Maybe t -> m (Maybe a)
mapJustM f (Just o) = f o
mapJustM _ Nothing  = return Nothing

findCommit :: Git -> Ref -> IO (Maybe GitObject)
findCommit git ref = findObject git ref True >>= mapJustM unwrap
    where unwrap c@(Commit _) = return $ Just c
          unwrap _            = return Nothing

findTree :: Git -> Ref -> IO (Maybe GitObject)
findTree git ref = findObject git ref True >>= mapJustM unwrap
    where unwrap c@(Tree _) = return $ Just c
          unwrap _          = return Nothing

-- | try to resolve a string to a specific commit ref
-- for example: HEAD, HEAD^, master~3, shortRef
resolveRevision :: Git -> Revision -> IO (Maybe Ref)
resolveRevision git (Revision prefix modifiers) = resolvePrefix >>= modf modifiers
    where
        resolvePrefix :: IO Ref
        resolvePrefix = resolveNamedPrefix fs >>= maybe resolvePrePrefix (return . Just) >>= maybe (return $ fromHexString prefix) return

        resolvePrePrefix :: IO (Maybe Ref)
        resolvePrePrefix = do
            refs <- findReferencesWithPrefix git prefix
            case refs of
                []  -> return Nothing
                [r] -> return (Just r)
                _   -> error "multiple references with this prefix"

        fs = [ (specialExists, specialRead), (tagExists, tagRead), (headExists, headRead) ]

        resolveNamedPrefix []     = return Nothing
        resolveNamedPrefix (x:xs) = do
            exists <- (fst x) (gitRepoPath git) prefix
            if exists
                then Just <$> (snd x) (gitRepoPath git) prefix
                else resolveNamedPrefix xs
    
        modf [] ref                  = return (Just ref)
        modf (RevModParent i:xs) ref = do
            parentRefs <- getParentRefs ref
            case i of
                0 -> error "revision modifier ^0 is not implemented"
                _ -> case drop (i - 1) parentRefs of
                    []    -> error "no such parent"
                    (p:_) -> modf xs p

        modf (RevModParentFirstN 1:xs) ref = modf (RevModParent 1:xs) ref
        modf (RevModParentFirstN n:xs) ref = do
            parentRefs <- getParentRefs ref
            modf (RevModParentFirstN (n-1):xs) (head parentRefs)
        modf (_:_) _ = error "unimplemented revision modifier"

        getParentRefs ref = do
            obj <- findCommit git ref
            case obj of
                Just (Commit (CommitInfo _ parents _ _ _)) -> return parents
                Just _ -> error "reference in commit chain doesn't exists"
                Nothing -> error "reference in commit chain doesn't exists"

-- | returns a tree from a ref that might be either a commit, a tree or a tag.
resolveTreeish :: Git -> Ref -> IO (Maybe GitObject)
resolveTreeish git ref = findObject git ref True >>= mapJustM recToTree where
    recToTree (Commit (CommitInfo tree _ _ _ _)) = resolveTreeish git tree
    recToTree (Tag (TagInfo tref _ _ _ _))    = resolveTreeish git tref
    recToTree t@(Tree _)            = return $ Just t
    recToTree _                                              = return Nothing

-- | build a hierarchy tree from a tree object
buildHTree :: Git -> GitObject  -> IO HTree
buildHTree git (Tree ents) = mapM resolveTree ents
    where resolveTree (perm, ent, ref) = do
               obj <- findObjectType git ref
               case obj of
                   Just TypeBlob -> return (perm, ent, TreeFile ref)
                   Just TypeTree -> do
                       ctree <- findTree git ref
                       case ctree of
                           Nothing -> error "unknown reference in tree object: no such child"
                           Just t  -> do
                               dir   <- buildHTree git t
                               return (perm, ent, TreeDir ref dir)
                   Just _        -> error "wrong type embedded in tree object"
                   Nothing       -> error "unknown reference in tree object"
buildHTree _git _ = error "Stoupid buildHTree"

-- | resolve the ref (tree or blob) related to a path at a specific commit ref
resolvePath :: Git -> Ref -> [ByteString] -> IO (Maybe Ref)
resolvePath git commitRef paths = do
    commit <- findCommit git commitRef
    case commit of
        Just (Commit (CommitInfo tree _ _ _ _)) -> resolve tree paths
        _                    -> error ("not a valid ref: " ++ show commitRef)
    where
        resolve :: Ref -> [ByteString] -> IO (Maybe Ref)
        resolve treeRef []     = return $ Just treeRef
        resolve treeRef (x:xs) = do
            tree <- findTree git treeRef
            case tree of
                Just (Tree ents) -> do
                    let cEnt = treeEntRef <$> findEnt x ents
                    if xs == []
                        then return cEnt
                        else maybe (return Nothing) (\z -> resolve z xs) cEnt
                _          -> error ("not a valid ref: " ++ show treeRef)

        findEnt x = find (\(_, b, _) -> b == x)
        treeEntRef (_,_,r) = r
    
-- | basic checks to see if a specific path looks like a git repo.
isRepo :: FilePath -> IO Bool
isRepo path = do
    dir     <- doesDirectoryExist path
    subDirs <- mapM (doesDirectoryExist . (path </>))
        ["branches","hooks","info"
        ,"logs","objects","refs"
        ,"refs"</>"heads","refs"</>"tags"]
    return $ and ([dir] ++ subDirs)

-- | initialize a new repository at a specific location.
initRepo :: FilePath -> IO ()
initRepo path = do
    exists <- doesDirectoryExist path
    when exists $ error "destination directory already exists"
    createDirectory path
    mapM_ (createDirectory . (path </>))
        ["branches","hooks","info"
        ,"logs","objects","refs"
        ,"refs"</>"heads","refs"</>"tags"]
