{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
module Data.Git.Repository
    ( Git
    , gitRepoPath
    , openRepo
    , closeRepo
    , withRepo
    , findRepository
    , findReference
    , findReferencesWithPrefix
    , findObject
    , resolveRevision
    , isRepo

    -- * Named
    -- ** Obtain a list of existing elements
    , getHead
    , getBranchNames
    , getTagNames
    , getRemoteNames
    , getRemoteBranchNames

    -- ** Querying for existence
	, doesHeadExist
	, doesTagExist

    -- ** Obtain the references of the elements
    , readBranch 
    , readTag
    , readRemoteBranch 
    ) where

import System.Directory
import System.FilePath

import Control.Applicative ((<$>))
import Control.Exception
import Control.Monad

import qualified Data.ByteString as B
{-import qualified Data.ByteString.Char8 as BC-}
import Data.Word
import Data.IORef
import Data.List ((\\), isPrefixOf)

import Data.Git.Delta
import Data.Git.FileReader
import Data.Git.Index
import Data.Git.Pack
import Data.Git.Object
import Data.Git.Revision
import Data.Git.Loose
import Data.Git.Ref
import Data.Git.Path

data IndexReader = IndexReader IndexHeader FileReader

-- | represent an git repo, with possibly already opened filereaders
-- for indexes and packs
data Git = Git
    { gitRepoPath  :: FilePath      -- ^ Access the path where the repository is
                                    -- placed
    , indexReaders :: IORef [(Ref, IndexReader)]
    , packReaders  :: IORef [(Ref, FileReader)]
    }

-- | open a new git repository context
openRepo :: FilePath -> IO Git
openRepo path = liftM2 (Git path) (newIORef []) (newIORef [])

-- | close a git repository context, closing all remaining fileReaders.
closeRepo :: Git -> IO ()
closeRepo (Git { indexReaders = ireaders, packReaders = preaders }) = do
    mapM_ (closeIndexReader . snd) =<< readIORef ireaders
    mapM_ (fileReaderClose . snd) =<< readIORef preaders
    where closeIndexReader (IndexReader _ fr) = fileReaderClose fr

-- | Find the nearest repository in the file tree starting
-- at the given directory
findRepository :: FilePath -> IO (Maybe FilePath)
findRepository = inner ""
  where inner prevDir dir | prevDir == dir || dir == "." = return Nothing
        inner _ dir = do
            isGitRepo <- isRepo $ dir </> ".git"
            if isGitRepo then return $ Just dir
                         else inner dir $ takeDirectory dir

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

readFromPack :: Git -> Ref -> Word64 -> IO (Maybe ObjectInfo)
readFromPack git pref o = do
    (reader, x) <- readRawFromPack git pref o
    resolve reader o x
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
                    base <- findObjectRaw git bref
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
findObjectRawAt :: Git -> ObjectLocation -> IO (Maybe ObjectInfo)
findObjectRawAt _   NotFound    = return Nothing
findObjectRawAt git (Loose ref) = Just . (\(h,d)-> ObjectInfo h d[]) <$> looseReadRaw (gitRepoPath git) ref
findObjectRawAt git (Packed pref o) = readFromPack git pref o

-- | get an object from repository
findObjectRaw :: Git -> Ref -> IO (Maybe ObjectInfo)
findObjectRaw git ref =
    findReference git ref >>= findObjectRawAt git

-- | Get an object from repository, with a sha1.
-- All delta are resolved internally
findObject :: Git -> Ref -> IO (Maybe GitObject)
findObject git ref =
  maybe Nothing toObject <$> findObjectRaw git ref
    where toObject (ObjectInfo { oiHeader = (ty, _, extra), oiData = objData }) =
                packObjectFromRaw (ty, extra, objData)

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
                [r] -> return (Just r)
                _   -> return Nothing

        fs = [(doesTagExist, readTag), (doesHeadExist, readBranch)]

        resolveNamedPrefix []     = return Nothing
        resolveNamedPrefix (x:xs) = do
            exists <- (fst x) git prefix
            if exists
                then Just <$> (snd x) git prefix
                else resolveNamedPrefix xs
    
        modf [] ref                  = return (Just ref)
        modf (RevModParent 0:_ )   _ = return Nothing
        modf (RevModParent i:xs) ref = do
            parentRefs <- getParentRefs ref
            case drop (i - 1) parentRefs of
               []    -> return Nothing -- error "no such parent"
               (p:_) -> modf xs p

        modf (RevModParentFirstN 1:xs) ref = modf (RevModParent 1:xs) ref
        modf (RevModParentFirstN n:xs) ref = do
            parentRefs <- getParentRefs ref
            if null parentRefs
               then return Nothing
               else modf (RevModParentFirstN (n-1):xs) $ head parentRefs
        modf (_:_) _ = return Nothing -- error "unimplemented revision modifier"

        getParentRefs ref = do
            obj <- findObject git ref
            case obj of
                Just (Commit (CommitInfo _ parents _ _ _)) -> return parents
                Just _  -> return [] -- error "reference in commit chain doesn't exists"
                Nothing -> return [] -- error "reference in commit chain doesn't exists"

-- | basic checks to see if a specific path looks like a git repo.
isRepo :: FilePath -> IO Bool
isRepo path = do
    dir     <- doesDirectoryExist path
    subDirs <- mapM (doesDirectoryExist . (path </>))
                    ["objects","refs"</>"heads","refs"</>"tags"]
    return $ and ([dir] ++ subDirs)

getDirectoryContentNoDots :: FilePath -> IO [String]
getDirectoryContentNoDots path = filter noDot <$> getDirectoryContents path
	where noDot = (not . isPrefixOf ".")

-- | Obtain the current head of the repository.
getHead :: Git -> IO (Maybe Ref)
getHead (Git { gitRepoPath = path }) = do
    content <- readFile $ path </> "HEAD"
    case content of
        ('r':'e':'f':':':' ':fname) ->
            Just <$> readRef (path </> head (lines fname))
        _ -> return Nothing


-- | This function will fetch the list of known local
-- branches
getBranchNames :: Git -> IO [String]
getBranchNames (Git { gitRepoPath = path }) =
    getDirectoryContentNoDots $ headsPath path

-- | This function will fetch the list of tags declared in the
-- repository.
getTagNames :: Git -> IO [String]
getTagNames (Git { gitRepoPath = path }) = 
    getDirectoryContentNoDots $ tagsPath path

-- | This functions will fetch the list of remotes (distant
-- repository) known in the repository.
getRemoteNames :: Git -> IO [String]
getRemoteNames (Git { gitRepoPath = path }) =
    getDirectoryContentNoDots $ remotesPath path

-- | Given a repository and remote name, will fetch all
-- the known branches
getRemoteBranchNames :: Git -> String -> IO [String]
getRemoteBranchNames (Git { gitRepoPath = path }) =
    getDirectoryContentNoDots . remotePath path

readRef :: FilePath -> IO Ref
readRef path = fromHex . B.take 40 <$> B.readFile path

-- | Query the git repository to see if a given branch exists
doesHeadExist :: Git -> String -> IO Bool
doesHeadExist (Git { gitRepoPath = repo }) = doesFileExist . headPath repo

-- | Query the git repository to see if a given tag exists
doesTagExist :: Git -> String -> IO Bool
doesTagExist (Git { gitRepoPath = repo }) = doesFileExist . tagPath repo

readTag :: Git -> String -> IO Ref
readTag (Git { gitRepoPath = repo }) = readRef . tagPath repo

readBranch :: Git -> String -> IO Ref
readBranch (Git { gitRepoPath = repo }) = readRef . headPath repo

readRemoteBranch :: Git -> String -> String -> IO Ref
readRemoteBranch (Git { gitRepoPath = repo }) branch =
    readRef . remoteEntPath repo branch

