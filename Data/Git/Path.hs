-- |
-- Module      : Data.Git.Path
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Path where

import System.FilePath
import System.Random
import Control.Applicative ((<$>))
import Data.Git.Ref

headsPath :: FilePath -> FilePath
headsPath gitRepo = gitRepo </> "refs" </> "heads"

tagsPath :: FilePath -> FilePath
tagsPath gitRepo  = gitRepo </> "refs" </> "tags"

remotesPath  :: FilePath -> FilePath
remotesPath gitRepo = gitRepo </> "refs" </> "remotes"

headPath :: FilePath -> FilePath -> FilePath
headPath gitRepo name = headsPath gitRepo </> name

tagPath :: FilePath -> FilePath -> FilePath
tagPath gitRepo name = tagsPath gitRepo </> name

remotePath :: FilePath -> FilePath -> FilePath
remotePath gitRepo name = remotesPath gitRepo </> name

specialPath :: FilePath -> FilePath -> FilePath
specialPath gitRepo name = gitRepo </> name

remoteEntPath :: FilePath -> FilePath -> FilePath -> FilePath
remoteEntPath gitRepo name ent = remotePath gitRepo name </> ent

packDirPath :: FilePath -> FilePath
packDirPath repoPath = repoPath </> "objects" </> "pack"

indexPath :: FilePath -> Ref -> FilePath
indexPath repoPath indexRef =
	packDirPath repoPath </> ("pack-" ++ toHexString indexRef ++ ".idx")

packPath :: FilePath -> Ref -> FilePath
packPath repoPath packRef =
	packDirPath repoPath </> ("pack-" ++ toHexString packRef ++ ".pack")

objectPath :: FilePath -> FilePath -> FilePath -> FilePath
objectPath repoPath d f = repoPath </> "objects" </> d </> f

objectPathOfRef :: FilePath -> Ref -> FilePath
objectPathOfRef repoPath ref = objectPath repoPath d f
	where (d,f) = toFilePathParts ref

objectTemporaryPath :: FilePath -> IO FilePath
objectTemporaryPath repoPath = do
	r <- fst . random <$> getStdGen :: IO Int
	return (repoPath </> "objects" </> ("tmp-" ++ show r ++ ".tmp"))

