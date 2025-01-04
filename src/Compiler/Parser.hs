module Compiler.Parser 
    ( addToContext
    , execRunicTopLevel
    , execRunicT
    , grabFromContext
    , tryPopToken
    , RunicT
    , RunicTopLevel
    ) where

import Data.Map as M ( empty, insert, lookup )
import Data.Text ( Text )
import Data.List ( uncons )
import Compiler.Internal ( RunicContext, RunicObject, Token )
import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Except ( runExceptT, throwError, ExceptT )
import Control.Monad.RWS ( asks, execRWST, get, lift, put, RWST )

{-|
    A monad transformer built for capturing grammar errors or useful
    data for computing values to add to the compiler's context.

    This monad transformer type also contains a context that  
-}
type RunicT m = RWST
    RunicContext            -- reader (outer context, empty in the case of the global context)
    [Text]                  -- writer (captures text from Expr's)
    (Token, [Token], RunicContext) -- state (last token processed and tokens left, local context)
    (ExceptT String m)      -- exception (possible error from grammatically incorrect phrase, i.e. mismatch)

{-|
    The top-level of the Runic Parser state machine, containing an 
    empty outer context (reader value), the token stream to be parsed
    (half of state value), a global, mutable context (other half of 
    state), and whose accompanying 'run' function produces a 
-}
type RunicTopLevel = RunicT Identity
-- (Almost) fully expanded, this is the type: 
-- RWST RunicContext [Text] (Token, [Token], RunicContext) ExceptT String Identity

{-|
    The function responsible for kicking off the global process of 
    parsing the Runic source code in a given program. 
-}
execRunicTopLevel :: RunicTopLevel a 
    -> [Token] 
    -> Either String (RunicContext, [Text])
execRunicTopLevel action tokens = do
    (initialToken, initialStream) <- maybe (Left "tried to compile \
        \source, but found no tokens") Right $ uncons tokens
    ((_, _, ctx), eqns) <- runIdentity $ runExceptT 
        $ execRWST action empty (initialToken, initialStream, empty)
    return (ctx, eqns)

{-|
    The function responsible for kicking off parser substates of the 
    main Runic compiler thread. This creates an isolated local context 
    while still providing access to the lower (more global?) context
    owned by the main thread.
-}
execRunicT :: Monad m 
    => RunicT m a -- the Runic monad transformer action to be executed, ignoring the returned value of type 'a'
    -> RunicContext -- the context to lift from the local level to the global level of the transformer-wrapped monad type
    -> Token -- the previous token that triggered this monad action in the overarching state machine
    -> [Token] -- the initial token stream to pass to the 
    -> m (Either String ([Token], RunicContext, [Text]))
execRunicT action ctx prevTok tokens = do
    result <- runExceptT $ execRWST action ctx (prevTok, tokens, empty)
    return $ fmap spliceState result
    where
        spliceState :: ((Token, [Token], RunicContext), [Text]) 
            -> ([Token], RunicContext, [Text])
        spliceState ((prev, inputQueue, localContext), captures) = 
            (prev:inputQueue, localContext, captures)

{-|
    
-}
tryPopToken :: Monad m => RunicT m (Maybe Token) 
tryPopToken = do
    (prev, inputQueue, localCtx) <- get
    case uncons inputQueue of
        Nothing -> return Nothing
        Just (h, tl) -> do
            put (prev, tl, localCtx)
            return (Just h)

{-|

-}
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

{-|

-}
addToContext :: Monad m => Text -> RunicObject -> RunicT m ()
addToContext name item = do
    (prev, inputQueue, localCtx) <- get 
    let newLocalCtx = insert name item localCtx
    put (prev, inputQueue, newLocalCtx)        
