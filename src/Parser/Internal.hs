{-# LANGUAGE Safe #-}
module Parser.Internal
    ( evalRunicT
    , runicAddToCtx
    , runicGetFromCtx
    ) where

import safe Control.Monad.Reader (runReaderT)
import safe qualified Data.Map as M (empty, insert, lookup)
import safe Text.Parsec (getState, runParserT, updateState, SourceName, ParseError)
import safe Types (CtxItem, RunicT, UCMap)

-- | Evaluates a @RunicT@ monad transformer action, returning the 
--   final state of the contained within the Runic parser.
evalRunicT :: Monad m
    => RunicT m ()
    -> UCMap
    -> SourceName
    -> String
    -> m (Either ParseError ())
evalRunicT action ucMap sourceName inputStream
    = flip runReaderT ucMap 
    $ runParserT action M.empty sourceName inputStream

-- | Handles adding items to the context folded up by the Runic 
--   parser.
runicAddToCtx :: Monad m => String -> CtxItem -> RunicT m ()
runicAddToCtx name item = updateState $ M.insert name item

-- | Handles getting values from the context stored in the Runic 
--   parser.
runicGetFromCtx :: Monad m => String -> RunicT m (Maybe CtxItem)
runicGetFromCtx name = M.lookup name <$> getState