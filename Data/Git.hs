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

               -- ** Querying for existence
               , doesHeadExist
               , doesTagExist

               -- ** Obtain the references of the elements
               , readBranch 
               , readTag
               , readRemoteBranch

               -- * Git objects
               , GitObject(..)
               , CommitAuthor(..)
               , CommitInfo(..)
               , TagInfo(..)
               , FileRights
               , TreeEntry

               -- * Reference conversion
               , Ref
               , toHexString
               , toBinary
               , fromHexString 
               , fromBinary
               , toHex
               , fromHex

               -- * Revisions
               , Revision
               , revFromString
               , resolveRevision
               ) where

import Data.Git.Object
import Data.Git.Ref
import Data.Git.Repository
import Data.Git.Revision

