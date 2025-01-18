{-# LANGUAGE Safe #-}
module Parser.Internal 
    ( runicAddToCtx
    , runicGetFromCtx
    ) where

import safe Prelude hiding (lookup)
import safe qualified Data.Map as Map (insert, lookup)
import safe Text.Parsec (getState, updateState)
import safe Types (CtxItem, RunicT)

-- runicGetUCData :: String -> 

-- | Handles adding items to the context folded up by the Runic 
--   parser.
runicAddToCtx :: Monad m => String -> CtxItem -> RunicT m ()
runicAddToCtx name item = updateState $ Map.insert name item

-- | Handles getting values from the context stored in the Runic 
--   parser.
runicGetFromCtx :: Monad m => String -> RunicT m (Maybe CtxItem)
runicGetFromCtx name = Map.lookup name <$> getState