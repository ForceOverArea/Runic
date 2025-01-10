{-# LANGUAGE Safe #-}
module Compiler.RunicParser.Internal
    ( (?>)
    , (??>)
    , (.>)
    , (..>)
    , addToContext
    , buildRegex
    , buildRegexOpt
    , cycleToken
    , execRunicTopLevel
    , execRunicT
    , getContext
    , getLastToken
    , tryGetFromCtx
    , optionValidator
    , runParser
    , tryGetToken
    , validator
    , RunicT
    , RunicTopLevel
    ) where

import Data.Map as M ( empty, insert, lookup, union )
import Data.Text ( Text )
import Data.List ( uncons )
import Control.Monad ( when )
import Control.Monad.Identity ( Identity(..) )
import Control.Monad.Except ( runExceptT, throwError, ExceptT )
import Control.Monad.RWS ( ask, asks, execRWST, get, lift, put, tell, RWST, MonadWriter )
import Compiler.RunicParser.Errors ( RunicParserError(..) )
import Compiler.RunicParser.Types
    ( getToken
    , tElem
    , RunicContext
    , RunicObject
    , Token
    , RunicKeyword(Expr)
    )

-- | A monad transformer built for capturing grammar errors or useful
-- data for computing values to add to the compiler's context.
--
-- This monad transformer type also contains a context that  
type RunicT m = RWST
    RunicContext            -- reader (outer context, empty in the case of the global context)
    [Text]                  -- writer (captures text from Expr's)
    (Token, [Token], RunicContext) -- state (last token processed and tokens left, local context)
    (ExceptT String m)      -- exception (possible error from grammatically incorrect phrase, i.e. mismatch)


-- | ### RunicTopLevel
--
-- The top-level of the Runic Parser state machine, containing an 
-- empty outer context (reader value), the token stream to be parsed
-- (half of state value), a global, mutable context (other half of 
-- state), and whose accompanying @eval@ function produces an error 
-- message or a final, global context and list of equations to solve.
-- 
-- ### What is it for? 
-- 
-- This information will be passed to the dependency graph builder to
-- create a solution to the system, which can be computed at a later 
-- step.
-- 
-- (Almost) fully expanded, this is the type: 
-- @RWST RunicContext [Text] (Token, [Token], RunicContext) ExceptT String Identity@
type RunicTopLevel = RunicT Identity

-- | The function responsible for kicking off the global process of 
-- parsing the Runic source code in a given program. 
execRunicTopLevel :: RunicTopLevel a
    -> [Token]
    -> Either String (RunicContext, [Text])
execRunicTopLevel action tokens = do
    (initialToken, initialStream) <- maybe 
        (Left "tried to compile source, but found no tokens") 
        Right $ uncons tokens
    ((_, _, ctx), eqns) <- runIdentity $ runExceptT
        $ execRWST action empty (initialToken, initialStream, empty)
    return (ctx, eqns)

-- | The function responsible for kicking off parser substates of the 
--   main Runic compiler thread. This creates an isolated local context 
--   while still providing access to the lower (more global?) context
--   owned by the main thread.
execRunicT :: Monad m
    => RunicT m a -- ^ The Runic monad transformer action to be executed, ignoring the returned value of type 'a'
    -> RunicContext -- ^ the context to lift from the local level to the global level of the transformer-wrapped monad type 
    -> Token -- ^ the previous token that triggered this monad action in the overarching state machine
    -> [Token] -- ^ the initial token stream to pass to the @RunicT m@ action
    -> m (Either String ((Token, [Token], RunicContext), [Text]))
execRunicT action ctx prevTok tokens = do
    runExceptT $ execRWST action ctx (prevTok, tokens, empty)

-- | Starts a sub-state machine of the global (or local function) 
--   parser, returning the expressions captured while running the 
--   given Runic-native regex pattern.
runParser :: Monad m => RunicT m () -> RunicT m [Text]
runParser runicKeywordRegex = do
    (prev, inputQueue, localCtx) <- get
    globalCtx <- ask
    let substateGlobalCtx = localCtx `union` globalCtx
    result <- lift . lift -- lift 2X for ExceptT AND RWST
        $ execRunicT runicKeywordRegex substateGlobalCtx prev inputQueue
    case result of 
        Left errMsg -> throwError errMsg
        Right (state, captures) -> do
            put state
            return captures
    
{-|
    
-}
cycleToken :: Monad m => RunicT m ()
cycleToken = do
    (_, inputQueue, localCtx) <- get
    case uncons inputQueue of
        Nothing -> return ()
        Just (h, tl) -> do
            put (h, tl, localCtx)

{-|

-}
tryGetToken :: Monad m => RunicT m (Maybe Token)
tryGetToken = do
    (_, inputQueue, _) <- get
    case uncons inputQueue of
        Nothing -> return Nothing
        Just (h, _) -> return (Just h)

{-|

-}
getLastToken :: Monad m => RunicT m Token
getLastToken = do
    (lt, _, _) <- get
    return lt

{-|
    A Runic-specific version of `tell` that logs an expression if the
    given token is an expression, otherwise this function adds 
    nothing to the log.
-}
logExpression :: Monad m => Token -> RunicT m ()
logExpression tok = do
    case getToken tok of
        (Expr e) -> tell [e]
        _ -> return ()

-- | Grabs the union of the state and the local context, favoring 
-- duplicate values defined in the local context. 
getContext :: Monad m => RunicT m RunicContext 
getContext = do
    (_, _, localCtx) <- get
    globalCtx <- ask
    return $ localCtx `union` globalCtx

-- | Tries to get a specific item from context by name (checking in
-- the local-favoring union of the local and global context)
tryGetFromCtx :: Monad m => Text -> RunicT m (Maybe RunicObject) 
tryGetFromCtx name = M.lookup name <$> getContext

{-|

-}
addToContext :: Monad m => Text -> RunicObject -> RunicT m ()
addToContext name item = do
    (prev, inputQueue, localCtx) <- get
    let newLocalCtx = insert name item localCtx
    put (prev, inputQueue, newLocalCtx)

{-|

-}
validator :: Monad m => [RunicKeyword] -> RunicT m ()
validator whitelist = do
    possTok <- tryGetToken
    case possTok of
        Nothing -> throwError $ show ""
        Just x -> do
            logExpression x
            if getToken x `tElem` whitelist || null whitelist then
                cycleToken
            else
                throwError $ show ""

{-|

-}
optionValidator :: Monad m => [RunicKeyword] -> RunicT m ()
optionValidator whitelist = do
    possTok <- tryGetToken
    case possTok of
        Nothing -> throwError $ show ""
        Just x -> do
            logExpression x
            when (getToken x `tElem` whitelist) 
                cycleToken

-- | A function for starting a regex in the Runic alphaber, matching
-- the token given once.
buildRegex :: Monad m => RunicKeyword -> RunicT m ()
buildRegex expected = validator [expected]

-- | A function for starting a regex in the Runic alphabet, optionally
-- matching the token given once.
buildRegexOpt :: Monad m => RunicKeyword -> RunicT m ()
buildRegexOpt expected = optionValidator [expected]

-- | The match whitelist operator for matching one of the following 
-- tokens given in a list once.
(..>) :: Monad m => RunicT m () -> [RunicKeyword] -> RunicT m ()
prevTokenValid ..> whitelist = prevTokenValid >> validator whitelist

-- | The match token operator for matching the following token once
(.>) :: Monad m => RunicT m () -> RunicKeyword -> RunicT m ()
prevTokenValid .> expected = prevTokenValid ..> [expected]

-- | The optional whitelist operator for optionally matching one of a
--   list of tokens.
(??>) :: Monad m
    => RunicT m ()
    -> [RunicKeyword]
    -> RunicT m ()
prevTokenValid ??> whitelist
    = prevTokenValid >> optionValidator whitelist

-- | The optional match operator for optionally matching a single 
--   token.
(?>) :: Monad m => RunicT m () -> RunicKeyword -> RunicT m ()
prevTokenValid ?> expected = prevTokenValid ??> [expected]