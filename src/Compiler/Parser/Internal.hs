{-# LANGUAGE Safe #-}
module Compiler.Parser.Internal 
    ( runicAddToCtx
    , runicGetFromCtx
    ) where

import safe Prelude hiding (lookup)
import safe Compiler.Internal (CtxItem, Runic)
import safe Data.Map (insert, lookup)
import safe Text.Parsec (getState, updateState)

-- | Handles adding items to the context folded up by the Runic 
--   parser.
runicAddToCtx :: String -> CtxItem -> Runic ()
runicAddToCtx name item = updateState $ insert name item

-- | Handles getting values from the context stored in the Runic 
--   parser.
runicGetFromCtx :: String -> Runic (Maybe CtxItem)
runicGetFromCtx name = lookup name <$> getState