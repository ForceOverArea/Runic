module Compiler.Parser 
    ( RunicT
    ) where

import Data.Map as M ( insert, lookup, Map )
import Data.Text ( Text )
import Data.List ( uncons )
import Compiler.Internal ( RunicObject, Token, RunicKeyword )
import Control.Monad.RWS ( asks, get, lift, put, RWST )
import Control.Monad.Except ( throwError, ExceptT )

{-|
    A monad transformer built for capturing grammar errors or useful
    data for computing values to add to the compiler's context.

    This monad transformer type also contains a context that  
-}
type RunicT m = RWST
    (Map Text RunicObject)  -- reader (outer context, empty in the case of the global context)
    [Text]                  -- writer (captures text from Expr's)
    (Token, [Token], Map Text RunicObject) -- state (last token processed and tokens left, local context)
    (ExceptT String m)      -- exception (possible error from grammatically incorrect phrase, i.e. mismatch)

tryPopToken :: Monad m => RunicT m (Maybe Token) 
tryPopToken = do
    (prev, inputQueue, localCtx) <- get
    case uncons inputQueue of
        Nothing -> return Nothing
        Just (h, tl) -> do
            put (prev, tl, localCtx)
            return (Just h)

grabFromContext :: Monad m => Text -> RunicT m RunicObject
grabFromContext name = do
    (_, _, localCtx) <- get
    -- Try to get the result from the local context first
    let localResult = M.lookup name localCtx
    case localResult of
        Just value -> return value
        Nothing -> do
            -- if that fails, then look to the global context in the 
            -- reader level of the monad stack, returning an error 
            -- message on failure.
            globalResult <- asks (M.lookup name)
            case globalResult of
                Just value -> return value
                Nothing -> lift $ throwError 
                    $ "found undefined value " 
                    ++ show name 
                    ++ " in expression"

addToContext :: Monad m => Text -> RunicObject -> RunicT m ()
addToContext name item = do
    (prev, inputQueue, localCtx) <- get 
    let newLocalCtx = insert name item localCtx
    put (prev, inputQueue, newLocalCtx)        

cycleToken :: Monad m => RunicT m ()
cycleToken = do
    token <- tryPopToken
    (_, inputQueue, localCtx) <- get 
    case token of
        Nothing -> return ()
        Just prevToken -> put (prevToken, inputQueue, localCtx)

runRunicT :: Monad m => RunicT m a -> Either String ()
runRunicT md =