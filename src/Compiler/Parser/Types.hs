{-
Defines the type hierarchy for data available in each superstate that 
the compiler may be in at any given point in time.
-}
{-# LANGUAGE InstanceSigs #-}
module Compiler.Parser.Types 
    ( CtxMap
    ) where

import Prelude hiding ( lookup )
import Compiler.Parser.Objects ( RToken )
import Control.Monad.State.Lazy ( get, lift, put, StateT )
import Data.Map ( insert, lookup, Map )
import Data.Text ( Text )

{-| 
An item that should be stashed as part of the context to a system
of equations.
-}
data CtxItem
    = CtxGuessDmn Double Double Double
    | CtxConst Text Double
    | CtxFunction Text Int (Int -> [Double] -> Double)

instance Eq CtxItem where
    (==) :: CtxItem -> CtxItem -> Bool
    (CtxGuessDmn a b c) == (CtxGuessDmn d e f) = 
        (a == d) && (b == e) && (c == f)
    (CtxConst namea a) == (CtxConst nameb b) = 
        (namea == nameb) && (a == b)
    (CtxFunction namea na _) == (CtxFunction nameb nb _) = 
        (na == nb) && (namea == nameb)
    _ == _ = False

instance Show CtxItem where
    show :: CtxItem -> String
    show (CtxFunction name n _) = "function " 
        ++ show name 
        ++ " = double[" 
        ++ show n 
        ++ "] -> double"
    show (CtxConst name a) = "constant " 
        ++ show name 
        ++ " = " 
        ++ show a
    show (CtxGuessDmn a b c) = "guess: " 
        ++ show a 
        ++ ", domain: [" 
        ++ show b 
        ++ ", " 
        ++ show c 
        ++ "]"

{-| 
A type alias for a map of names (Text) to their corresponding 
Runic-relevant data.
-}
type CtxMap = Map Text CtxItem

{-|
A type alias for the state tracked by the global parser superstate.
This holds both the context that will be used in solving the system as
well as the remaining tokens to be parsed.
-}
type GlobalState a = (CtxMap, [a])

-- | A type alias for the `StateT Either String` monad. 
type GlobalStateM = StateT (GlobalState RToken) (Either String)

-- | Returns the global context.
getCtx :: GlobalStateM CtxMap
getCtx = do
    (ctx, _) <- get
    return ctx

-- | Sets the global context to the given value.
setCtx :: CtxMap -> GlobalStateM ()
setCtx ctx = do
    (_, remain) <- get 
    put (ctx, remain)

{-| 
Tries to add a given value to the global context, returning an error 
message on failure.
-}
tryAddToCtx :: Text -> CtxItem -> GlobalStateM ()
tryAddToCtx name item = do
    ctx <- getCtx
    case lookup name ctx of
        Just existing -> lift (Left $ "found name collision: " 
            ++ show name 
            ++ " is already a value of type "
            ++ show existing)
        Nothing -> setCtx (insert name item ctx)

-- | Pops a token off of the 
pop :: GlobalStateM RToken
pop = do
    (ctx, remain) <- get
    if null remain
        then lift (Left "expected token, but stack was empty")
        else do
            put (ctx, tail remain)
            return (head remain)

push :: RToken -> GlobalStateM ()
push tok = do
    (ctx, remain) <- get
    put (ctx, tok:remain)

-- | A type alias for the `StateT [RToken] StateT Either String` monad.
type ParserStateM a = StateT [RToken] GlobalStateM a

-- | Stashes a given token value.
stashToken :: RToken -> ParserStateM ()
stashToken tok = do
    stash <- get
    put (stash ++ [tok])

-- | Same as `pop` but in the Parser level of the monad stack.
parserPop :: ParserStateM RToken
parserPop = do lift pop

-- | Same as `push` but in the Parser level of the monad stack.
parserPush :: RToken -> ParserStateM ()
parserPush tok = do lift $ push tok


