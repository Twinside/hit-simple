-- | Main module to manipulate a git repository.
module Data.Git( 
                -- * Repository
                 Git
               , gitRepoPath
               , openRepo
               , closeRepo
               , withRepo
               , findRepository

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
