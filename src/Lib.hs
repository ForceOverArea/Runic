module Lib
    ( evalRunic
    ) where

import Control.Arrow ( arr, first, (>>>), (&&&) )
import Control.Exception ( )
import Data.Map ( Map ) 
import Data.Text ( Text ) 

import Tokenizer ( initLineNoMap, runicTokenize, RunicItem )

evalRunic :: Text -> ([RunicItem], Map Int Text)
evalRunic = 
    arr initLineNoMap
    >>> runicTokenize &&& id
    >>> first 
    
