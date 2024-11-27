module Compiler
    ( 
    ) where

import Control.Arrow ( arr, (>>>) )
import Control.Monad.State.Lazy ( evalState, get, put, State )
import Data.Map ( insert, member, Map )
import Data.Text ( intercalate, pack, split, toLower, unpack, Text, toLower )
import Text.Read ( readMaybe )

import Tokenizer ( RunicItem(..), RunicToken(..) )
import Domain ( validateDomainDecl )
import Guess ( validateGuessDecl )