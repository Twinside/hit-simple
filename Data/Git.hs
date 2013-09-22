-- | Main module to manipulate a git repository.
module Data.Git( 
                -- * Repository
                 Git
               , gitRepoPath
               , openRepo
               , closeRepo
               , withRepo
               , findRepository

               -- * Most important question
               , findObject

               -- * Find named elements
               -- ** Obtain a list of existing elements
               , getHead
               , getBranchNames
               , getTagNames
               , getRemoteNames
               , getRemoteBranchNames
               , getGitSvnBranchNames

               -- ** Querying for existence
               , doesHeadExist
               , doesTagExist
               , doesRemoteHeadExist

               -- ** Obtain the references of the elements
               , readBranch 
               , readTag
               , readRemoteBranch
               , readGitSvnBranch 

               -- * Git objects
               , GitObject(..)
               , CommitAuthor(..)
               , CommitInfo(..)
               , TagInfo(..)
               , FileRights
               , TreeEntry

               -- * Reference conversion
               , Ref
               , RefSpec( .. )
               , toHexString
               , toBinary
               , fromHexString 
               , fromHexText
               , fromBinary
               , toHex
               , fromHex

               -- * Revisions
               , Revision
               , revFromString
               , resolveRevision
               , readAllRemoteBranches
               ) where

import Data.Git.Object
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Revision

