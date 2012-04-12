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

               -- * Git objects
               , GitObject(..)
               , CommitAuthor(..)
               , CommitInfo(..)
               , TagInfo(..)
               , FileRights
               , TreeEntry
               ) where

import Data.Git.Repository
import Data.Git.Object
