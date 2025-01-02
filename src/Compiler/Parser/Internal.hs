{-# LANGUAGE Safe #-}

module Compiler.Parser.Internal
    ( (=+=)
    , returnError
    , returnMatchError
    , runGenRegexT
    , stashToken
    , tElem
    , tryPopQueue
    , GenRegexT
    , TokenEq
    ) where

import Control.Monad.Except ( throwError, ExceptT, runExceptT )
import Control.Monad.RWS ( execRWST, RWST )
import Control.Monad.State.Lazy ( get, put )
import Control.Monad.Writer.Lazy ( lift )
import Data.List ( uncons )

{-|
    A monad for an alphabet-generic Regex that can capture tokens it 
    has parsed.
-}
type GenRegexT t m = RWST () [t] [t] (ExceptT String m)

{-|

-}
class TokenEq a where
    {-|
        The token equality operator, which allows a second definition
        of Eq to be defined for a token type that implements this 
        class.
    -}
    (=+=) :: a -> a -> Bool

{-|

-}
tElem :: (Foldable t, TokenEq a) => a -> t a -> Bool
tElem needle = foldl findNeedle False
    where
        findNeedle wasFound tok = needle =+= tok || wasFound

{-|
    Pops a value off of the input stack, returning either `Just` the 
    value or `Nothing`.
-}
tryPopQueue :: Monad m => GenRegexT a m (Maybe a)
tryPopQueue = do
    tokens <- get
    case uncons tokens of
        Nothing -> return Nothing
        Just (x, xs) -> do
            put xs
            return (Just x)

{-|

-}
returnMatchError :: (Monad m, Show a) => a -> a -> GenRegexT a m b
returnMatchError expected actual = returnError
    $ "expected "
    ++ show expected
    ++ ", but found "
    ++ show actual

{-|
    A factory function that returns a closure who validates a token 
    against an expected value in a alphabet-agnostic regex pattern. 

    I.e. given an expected token, this function returns a validator
    that either returns an error message displaying the expected 
    token or a `GenRegex a` monad.
-}
stashToken :: (Show a, TokenEq a) => a -> a -> GenRegexT a m a
stashToken expected actual = do
    if expected =+= actual then
        return actual
    else
        returnMatchError expected actual

{-|

-}
runGenRegexT :: Monad m => GenRegexT a m b -> [a] -> m (Either String ([a], [a]))
runGenRegexT md tokens = runExceptT $ execRWST md () tokens