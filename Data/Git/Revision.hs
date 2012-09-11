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
    deriving (Eq, Show)

-- | A revision is complex git accessor like `HEAD~3`
data Revision = Revision String [RevModifier]
    deriving (Eq, Show)

-- | Parse the following Git references
--
-- syntax is REF, the current reference (branch, HEAD...)
-- and accept a combinaison of the following modifiers :
--
--  * REF^n where n represent the nieth parent
                    --
--  * REF~n where n represent the nieth first parent
                    --
--  * REF\@\{type} where type can be `tree`, `commit`, `blob` or `tag`
                           --
--  * REF\@\{date}
--
--  * REF\@\{n}
--
revFromString :: String -> Either String Revision
revFromString s = case parse parser "" s of
        Left err -> Left $ show err
        Right v  -> Right v
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
               RevModAtType <$> choice (map string ["tree","commit","blob","tag"])
        
           parseAtN = try $ RevModAtN . read <$> many1 digit
        
           parseAtDate = try $ RevModAtDate <$> many (noneOf "}")
        
