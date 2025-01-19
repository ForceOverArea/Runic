{-# LANGUAGE Safe #-}
module Parser 
    ( -- runicParser
    ) where

import safe Types (RnCtx, RunicT)
-- import safe Parser.Lexemes (constDecl, domainDecl, guessDecl)
import safe Text.Parsec ((<|>), getState)

-- runicParser :: Monad m => RunicT m RnCtx
-- runicParser = do 
--     constDecl <|> guessDecl <|> domainDecl -- <|> functionDecl
--     getState -- return the final state as created by the parsing process