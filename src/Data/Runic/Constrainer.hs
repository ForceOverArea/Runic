{-# LANGUAGE Trustworthy #-}
module Data.Runic.Constrainer 
    ( getUnknowns
    , ConstrainT
    ) where

import safe Control.Monad.Reader (ReaderT)
import safe Control.Monad.State (StateT)
import safe Data.Map (member)
import safe Data.Runic.Types (RnCtx)
import safe Data.Tree (Tree)
import Text.Regex.TDFA ((=~), getAllTextMatches)

-- DUDE WE NEED TO USE LENSES

-- | The monad transformer used by the Runic problem constrainer
--   it consists of a read-only @RnCtx@ map and a mutable list of 
--   strings to identify which variables have been constrained.
type ConstrainT m = StateT (Tree String, [String]) (ReaderT RnCtx m)

identifierRegex :: String
identifierRegex = "[a-zA-Z_][a-zA-Z0-9_]*"

getVariables :: String -> [String]
getVariables = getAllTextMatches . (=~ identifierRegex)

getUnknowns :: RnCtx -> String -> [String]
getUnknowns ctx = filter (`member` ctx) . getVariables 

