module Compiler.Parser
    ( (<?->)
    , (<?+>)
    , (<?:->)
    , (<?:+>)
    , liftParser
    , liftStashParser
    , ParserState(..)
    ) where

import Control.Monad.State.Lazy ( get, lift, put, StateT )

-- | Type alias for a StateT monad.
type ParserStateTrans a = (StateT (ParserState a) (Either String) ())

-- | ParserState [stash] [remaining]
data ParserState a = ParserState [a] [a]
    deriving (Eq, Show)

-- | Pops a token from the remaining tokens stack. 
pop :: StateT (ParserState a) (Either String) a
pop = do
    ParserState stsh remain <- get
    if null remain
        then lift $ Left "ligma"
        else do
            put $ ParserState stsh (tail remain)
            return (head remain)

-- | Puts the given token at the end of the token stash queue.
stash :: a -> ParserStateTrans a
stash token = do
    ParserState stsh ts <- get
    put $ ParserState (stsh ++ [token]) ts
    return ()

-- | Creates a capture function that stashes whitelisted token, and 
--   fails on any other token. An empty whitelist allows any token.
stashValidator :: Eq a => [a] -> String -> ParserStateTrans a
stashValidator whitelist errMsg = do
    x <- pop
    if x `elem` whitelist || null whitelist
        then do stash x
        else do lift (Left errMsg)

-- | Creates a capture function that ignores a whitelisted token, and 
--   fails on any other token. An empty whitelist allows any token.
validator :: Eq a => [a] -> String -> ParserStateTrans a
validator whitelist errMsg = do
    x <- pop
    if x `elem` whitelist || null whitelist
        then return ()
        else do lift (Left errMsg)

-- | Lifts a token into a token validator that ignores the token upon 
--   validation.
liftParser :: Eq a => a -> ParserStateTrans a
liftParser tok = validator [tok] ""

-- | Lifts a token into a token validator that stashes the token upon 
--   validation.
liftStashParser :: Eq a => a -> ParserStateTrans a
liftStashParser tok = stashValidator [tok] ""

-- | The pass-through validation operator for creating a token 
--   validator that ignores the token on validation or returns an 
--   error message.
(<?->) :: Eq a => ParserStateTrans a -> a -> ParserStateTrans a
prevTokenValid <?-> nextToken = prevTokenValid <?:-> [nextToken]

-- | The pass-through list validation operator for creating a token 
--   validator that ignores the token on validation or returns an 
--   error message.
(<?:->) :: Eq a => ParserStateTrans a -> [a] -> ParserStateTrans a
prevTokenValid <?:-> nextTokens = prevTokenValid >> validator nextTokens ""

-- | The pass-through validation operator for creating a token 
--   validator that captures the token on validation or returns an 
--   error message.
(<?+>) :: Eq a => ParserStateTrans a -> a -> ParserStateTrans a
prevTokenValid <?+> nextToken = prevTokenValid <?:+> [nextToken]

-- | The pass-through list validation operator for creating a token 
--   validator that captures the token on validation or returns an 
--   error message.
(<?:+>) :: Eq a => ParserStateTrans a -> [a] -> ParserStateTrans a
prevTokenValid <?:+> nextTokens = prevTokenValid >> stashValidator nextTokens ""
