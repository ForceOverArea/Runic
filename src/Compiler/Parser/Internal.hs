{-# LANGUAGE Safe #-}
module Compiler.Parser.Internal 
    ( runicAddToCtx
    , runicGetFromCtx
    ) where

import safe Prelude hiding (lookup)
import safe Compiler.Internal (CtxItem, Runic)
import safe qualified Data.Map as Map (insert, lookup)
import safe Text.Parsec (getState, updateState)

-- | Handles adding items to the context folded up by the Runic 
--   parser.
runicAddToCtx :: String -> CtxItem -> Runic ()
runicAddToCtx name item = updateState $ Map.insert name item

-- | Handles getting values from the context stored in the Runic 
--   parser.
runicGetFromCtx :: String -> Runic (Maybe CtxItem)
runicGetFromCtx name = Map.lookup name <$> getState