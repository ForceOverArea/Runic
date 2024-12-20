{-|
    A module for a minimal implementation of Regex, but for a generic
    token type. This eases the process of adding new syntax to the 
    Runic compiler/parser.
-}
module Compiler.Parser.Factory
    ( (<->)
    , (<+>)
    , (<?>)
    , (|-|)
    , (|+|)
    , (|?|)
    , liftParser
    , liftStashParser
    , liftOptParser
    , parseExpression
    , tElem
    , tEqual
    , ParserState(..)
    , ParserStateTrans
    , TokenEq
    ) where

import Control.Monad.State.Lazy ( execStateT, get, lift, put, StateT )

{-|
A class for tokens to be used with the Runic parser factory. This 
class requires a definition for `tEqual` and provides a derived 
implementation for `tElem`.
-}
class TokenEq a where
    tEqual :: a -> a -> Bool
    
-- | Similar to `elem` but uses TokenEq's equality operator. 
tElem :: (Foldable t, TokenEq a) => a -> t a -> Bool
tElem needle = foldl findNeedle False
    where 
        findNeedle wasFound tok = needle `tEqual` tok || wasFound

-- | Type alias for a StateT monad used in the Runic parser factory.
type ParserStateTrans a = (StateT (ParserState a) (Either String) ())

-- | ParserState lastToken [stash] [remaining]
data ParserState a = ParserState a [a] [a]
    deriving (Eq, Ord, Show)

-- | Pops a token from the remaining tokens stack.
pop :: StateT (ParserState a) (Either String) a
pop = do
    ParserState lt stsh remain <- get
    if null remain
        then lift $ Left "tried to pop token value but stack was empty"
        else do
            put $ ParserState lt stsh (tail remain)
            return (head remain)

-- | Pushes a token onto the remaining tokens stack.
push :: a -> StateT (ParserState a) (Either String) ()
push token = do
    ParserState lt stsh ts <- get
    put $ ParserState lt stsh (ts ++ [token])

-- | Puts the given token at the end of the token stash queue.
stash :: a -> ParserStateTrans a
stash token = do
    ParserState lt stsh ts <- get
    put $ ParserState lt (stsh ++ [token]) ts

-- | Sets a token as the last token read.
setLastToken :: a -> ParserStateTrans a
setLastToken token = do
    ParserState _ stsh ts <- get
    put $ ParserState token stsh ts

-- | Retrieves the token in the "previous token" slot.
getLastToken :: StateT (ParserState a) (Either String) a
getLastToken = do
    ParserState lt _ _ <- get
    return lt

{-| 
Creates a capture function that ignores a whitelisted token, and 
fails on any other token. An empty whitelist allows any token.
-}
validator :: TokenEq a => [a] -> (a -> a -> String) -> ParserStateTrans a
validator whitelist errMsg = do
    x <- pop
    if x `tElem` whitelist || null whitelist
        then 
            setLastToken x
        else do 
            lt <- getLastToken
            lift (Left $ errMsg lt x)

{-| 
Creates a capture function that stashes a whitelisted token, and 
fails on any other token. An empty whitelist allows any token.
-}
stashValidator :: TokenEq a => [a] -> (a -> a -> String) -> ParserStateTrans a
stashValidator whitelist errMsg = do
    x <- pop
    if x `tElem` whitelist || null whitelist
        then do 
            stash x 
            setLastToken x
        else do 
            lt <- getLastToken
            lift (Left $ errMsg lt x)

{-| 
Creates a capture function that tries to validate a token, but 
does nothing if the token is not the expected value.
This may be useful if a token may not be present in a phrase that is 
still "grammatically correct".
-}
optionValidator :: TokenEq a => [a] -> ParserStateTrans a
optionValidator whitelist = do
    x <- pop
    if x `tElem` whitelist || null whitelist
        then do 
            setLastToken x
        else do 
            push x

{-|
Tries to find a token pattern in a given stack of token values, 
returning any captured values and the remaining portion of the stack
on a successful match. On failure, an error message is returned.
-}
parseExpression :: ParserStateTrans a -> a -> [a] -> Either String (ParserState a)
parseExpression pattern prevToken stack = 
    execStateT pattern (ParserState prevToken [] stack)

{-|
Template for a parse error message for matching against a single token.
-}
parseErrorMsg :: Show a => a -> a -> a -> String
parseErrorMsg expectTok prevTok currentTok = "expected " 
    ++ show expectTok 
    ++ " after " 
    ++ show prevTok
    ++ ", but found " 
    ++ show currentTok

{-|
Template for a parse error message for matching against one of many 
tokens.
-}
parseManyErrorMsg :: Show a => [a] -> a -> a -> String
parseManyErrorMsg expectTok prevTok currentTok = "expected one of " 
    ++ show expectTok 
    ++ " after " 
    ++ show prevTok
    ++ ", but found " 
    ++ show currentTok

{-|
Lifts a token into a token validator that ignores the token upon 
validation.
-}
liftParser :: (TokenEq a, Show a) => a -> ParserStateTrans a
liftParser tok = validator [tok] (parseErrorMsg tok)

{-| 
Lifts a token into a token validator that stashes the token upon 
validation.
-}
liftStashParser :: (TokenEq a, Show a) => a -> ParserStateTrans a
liftStashParser tok = stashValidator [tok] (parseErrorMsg tok)

{-| 
Lifts a token into a token validator that ignores the token upon
validation or proceeds to the next step.
-}
liftOptParser :: TokenEq a => a -> ParserStateTrans a
liftOptParser tok = optionValidator [tok]

{-| 
The pass-through validation operator for creating a token validator 
that ignores the token on validation or returns an error message.
-}
(<->) :: (TokenEq a, Show a) => ParserStateTrans a -> a -> ParserStateTrans a
prevTokenValid <-> nextToken = prevTokenValid 
    >> validator [nextToken] (parseErrorMsg nextToken)

{-| 
The capture validation operator for creating a token validator that 
captures the token on validation or returns an error message.
-}
(<+>) :: (TokenEq a, Show a) => ParserStateTrans a -> a -> ParserStateTrans a
prevTokenValid <+> nextToken = prevTokenValid 
    >> stashValidator [nextToken] (parseErrorMsg nextToken)

{-| 
The optional validation operator for creating a token validator that 
ignores the token on validation or leaves the proceeds to the next 
step in the validation process.
-}
(<?>) :: TokenEq a => ParserStateTrans a -> a -> ParserStateTrans a
prevTokenValid <?> nextToken = prevTokenValid 
    >> optionValidator [nextToken]

{-| 
The pass-through list validation operator for creating a token 
validator that ignores the token on validation or returns an error 
message.
-}
(|-|) :: (TokenEq a, Show a) => ParserStateTrans a -> [a] -> ParserStateTrans a
prevTokenValid |-| nextTokens = prevTokenValid 
    >> validator nextTokens (parseManyErrorMsg nextTokens)

{-| 
The capture list validation operator for creating a token validator 
that captures the token on validation or returns an error message.
-}
(|+|) :: (TokenEq a, Show a) => ParserStateTrans a -> [a] -> ParserStateTrans a
prevTokenValid |+| nextTokens = prevTokenValid 
    >> stashValidator nextTokens (parseManyErrorMsg nextTokens)

{-|
The optional list validation operator for creating a token validator 
that ignores the token on validation or proceeds to the next step in 
the validation process.
-}
(|?|) :: TokenEq a => ParserStateTrans a -> [a] -> ParserStateTrans a
prevTokenValid |?| nextTokens = prevTokenValid 
    >> optionValidator nextTokens
