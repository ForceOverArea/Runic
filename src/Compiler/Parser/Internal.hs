module Compiler.Parser.Internal 
    ( RegexT
    , TokenEq
    ) where

import Control.Monad.Writer.Lazy ( WriterT )

type RegexT a = WriterT [a] (Either String)

class TokenEq a where
    {-|
        The token equality operator, which allows a second definition
        of Eq to be defined for a token type that implements this 
        class.
    -}
    (=+=) :: a -> a -> Bool

