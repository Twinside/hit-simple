-- |
-- Module      : Data.Git.Delta
-- License     : BSD-style
-- Maintainer  : Vincent Hanquez <vincent@snarc.org>
-- Stability   : experimental
-- Portability : unix
--
module Data.Git.Revision
    ( Revision(..)
    , RevModifier(..)
    , revFromString
    ) where

import Control.Applicative( (<$>), (<*>), (*>), (<*) )
import Text.Parsec( parse, try, many1, char
                  , optionMaybe, string
                  , choice, many, noneOf, digit )

data RevModifier =
      RevModParent Int       -- ^ parent accessor ^<n> and ^
    | RevModParentFirstN Int -- ^ parent accessor ~<n>
    | RevModAtType String    -- ^ @{type} accessor
    | RevModAtDate String    -- ^ @{date} accessor
    | RevModAtN Int          -- ^ @{n} accessor
    deriving (Eq)

data Revision = Revision String [RevModifier]
    deriving (Eq)

revFromString :: String -> Revision
revFromString s = either (error.show) id $ parse parser "" s
     where parser = Revision <$> many (noneOf "^~@")
                             <*> many (choice [parseParent, parseFirstParent, parseAt])

           parseParent = try $
               RevModParent . maybe 1 read <$> (char '^' *> optionMaybe (many1 digit))
        
           parseFirstParent = try $
               RevModParentFirstN . read <$> (char '~' *> many1 digit)
        
           parseAt = try $ 
               (char '@' *> char '{' 
                         *> choice [ parseAtType, parseAtDate, parseAtN ])
                         <* char '}'
        
           parseAtType = try $
               RevModAtType <$> (choice $ map string ["tree","commit","blob","tag"])
        
           parseAtN = try $ RevModAtN . read <$> many1 digit
        
           parseAtDate = try $ RevModAtDate <$> many (noneOf "}")
        
