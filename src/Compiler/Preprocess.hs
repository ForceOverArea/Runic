{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE InstanceSigs #-}

module Compiler.Preprocess 
    ( tokenizeSource
    ) where

import Prelude hiding ( lookup, words )
import Compiler.Objects ( tokenMapping, Token(..) )
import Control.Arrow ( (>>>), (&&&), arr, first )
import Data.Map ( fromList, keys, lookup, Map )
import Data.Text ( replace, split, words, Text )

-- | A token with line number it originated from
data RunicToken a = RunicToken Int a

instance Functor RunicToken where
    fmap :: (a -> b) -> RunicToken a -> RunicToken b
    fmap f (RunicToken ln rt) = RunicToken ln $ f rt

-- | Enumerates lines of text with their line numbers starting at 1
getLineNumbers :: [Text] -> [(Int, Text)]
getLineNumbers lns = zip [1..length lns + 1] lns

{-|
Looks for all tokens defined in `tokenMapping` (i.e. all legal 
keywords in the Runic language) and pads them with additional 
whitespace. This guarantees they are delimited by spaces to create a 
list of tokens in a block of text.
-}
punctuate :: Text -> Text
punctuate txt = foldr puncKw txt (keys tokenMapping)
    where
        puncKw :: Text -> Text -> Text
        puncKw wrd = replace wrd (" " <> wrd <> " ")

-- | Converts a block of text to a token or an expression.
tokenize :: Text -> Token
tokenize t = case lookup t tokenMapping of
    Just tok -> tok
    Nothing -> Expr t

-- | Converts a line of text to a list of RunicTokens.
tokenizeLine :: Int -> Text -> [RunicToken Token]
tokenizeLine ln t = 
    map (RunicToken ln . tokenize) (words $ punctuate t) ++ [RunicToken ln NewLine]

{-|
Produces a list of all tokens in the given block of text as well as a 
map of line numbers to their contained text. In this case the block 
is the Runic source code.
-}
tokenizeSource :: Text -> ([RunicToken Token], Map Int Text)
tokenizeSource = 
    arr split (== '\n')
    >>> getLineNumbers
    >>> map (uncurry tokenizeLine) &&& fromList
    >>> first concat