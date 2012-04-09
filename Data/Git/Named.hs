{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Data.Git.Named
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Named
	( headList
	, headExists
	, headRead
	, headWrite
	, remotesList
	, remoteList
	, tagList
	, tagExists
	, tagRead
	, tagWrite
	, specialRead
	, specialExists
	) where

import Control.Applicative ((<$>))

import System.Directory

import Data.Git.Path
import Data.Git.Ref
import Data.List (isPrefixOf)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

getDirectoryContentNoDots :: FilePath -> IO [[Char]]
getDirectoryContentNoDots path = filter noDot <$> getDirectoryContents path
	where noDot = (not . isPrefixOf ".")

headList :: FilePath -> IO [[Char]]
headList gitRepo = getDirectoryContentNoDots (headsPath gitRepo)

tagList :: FilePath -> IO [[Char]]
tagList gitRepo = getDirectoryContentNoDots (tagsPath gitRepo)

remotesList :: FilePath -> IO [[Char]]
remotesList gitRepo = getDirectoryContentNoDots (remotesPath gitRepo)

remoteList :: FilePath -> FilePath -> IO [[Char]]
remoteList gitRepo remote = getDirectoryContentNoDots (remotePath gitRepo remote)

writeRef :: FilePath -> Ref -> IO ()
writeRef path ref = B.writeFile path (B.concat [toHex ref, B.singleton 0xa])

readRef :: FilePath -> IO Ref
readRef path = fromHex . B.take 40 <$> B.readFile path

readRefAndFollow :: String -> FilePath -> IO Ref
readRefAndFollow gitRepo path = do
	content <- B.readFile path
	if "ref: " `B.isPrefixOf` content
		then do -- BC.unpack should be utf8.toString, and the whole thing is really fragile. need to do the proper thing.
			let file = BC.unpack $ BC.init $ B.drop 5 content
			readRefAndFollow gitRepo (gitRepo ++ "/" ++ file)
		else return (fromHex $ B.take 40 content)

headExists :: FilePath -> FilePath -> IO Bool
headExists gitRepo name    = doesFileExist (headPath gitRepo name)

headRead :: FilePath -> FilePath -> IO Ref
headRead gitRepo name      = readRef (headPath gitRepo name)

headWrite :: FilePath -> FilePath -> Ref -> IO ()
headWrite gitRepo name ref = writeRef (headPath gitRepo name) ref

tagExists :: FilePath -> FilePath -> IO Bool
tagExists gitRepo name    = doesFileExist (tagPath gitRepo name)

tagRead :: FilePath -> FilePath -> IO Ref
tagRead gitRepo name      = readRef (tagPath gitRepo name)

tagWrite :: FilePath -> FilePath -> Ref -> IO ()
tagWrite gitRepo name ref = writeRef (tagPath gitRepo name) ref

specialRead :: [Char] -> FilePath -> IO Ref
specialRead gitRepo name   = readRefAndFollow gitRepo (specialPath gitRepo name)

specialExists :: FilePath -> FilePath -> IO Bool
specialExists gitRepo name = doesFileExist (specialPath gitRepo name)

