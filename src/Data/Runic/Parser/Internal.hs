{-# LANGUAGE Safe #-}
module Data.Runic.Parser.Internal
    ( evalRunicT
    , evalRunic
    , runRunicT
    , runRunic
    , runicAddToCtx
    , runicGetFromCtx
    ) where

import safe Control.Monad.Reader (runReaderT)
import safe qualified Data.Map as M (empty, insert, lookup)
import safe Data.Runic.Types (CtxItem, RnCtx, RunicT, UCMap)
import safe Text.Parsec (getState, runParserT, updateState, SourceName, ParseError)
import Control.Monad.Identity (Identity (runIdentity))

-- | Performs a @Runic@ monad transformer action, returning the final
--   value returned by the action. 
runRunicT :: Monad m => RunicT m a -> UCMap -> SourceName -> String -> m (Either ParseError (a, RnCtx))
runRunicT action ucMap sourceName inputStream
    = flip runReaderT ucMap
    $ runParserT action' M.empty sourceName inputStream
    where
        action' = do
            result <- action
            finalState <- getState
            return (result, finalState)

-- | Performs a @Runic@ monad action, returning the final value 
--   returned by the action. 
runRunic :: RunicT Identity a -> UCMap -> SourceName -> String -> Either ParseError (a, RnCtx)
runRunic action ucMap sourceName inputStream =
    runIdentity $ runRunicT action ucMap sourceName inputStream

-- | Evaluates a @RunicT@ monad transformer action, returning the 
--   final state of the context contained within the Runic parser.
evalRunicT :: Monad m
    => RunicT m a
    -> UCMap
    -> SourceName
    -> String
    -> m (Either ParseError RnCtx)
evalRunicT action ucMap sourceName inputStream = do
    result <- runRunicT action ucMap sourceName inputStream
    return $ snd <$> result

-- | Evaluates a @Runic@ monad action, returning the final state of 
--   the context contained within the Runic parser.
evalRunic :: RunicT Identity () -> UCMap -> SourceName -> String -> Either ParseError RnCtx
evalRunic action ucMap sourceName inputStream =
    runIdentity $ evalRunicT action ucMap sourceName inputStream

-- | Handles adding items to the context folded up by the Runic 
--   parser.
runicAddToCtx :: Monad m => String -> CtxItem -> RunicT m ()
runicAddToCtx name item = updateState $ M.insert name item

-- | Handles getting values from the context stored in the Runic 
--   parser.
runicGetFromCtx :: Monad m => String -> RunicT m (Maybe CtxItem)
runicGetFromCtx name = M.lookup name <$> getState