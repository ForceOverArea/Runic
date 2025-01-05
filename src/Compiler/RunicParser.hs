{-# LANGUAGE Safe #-}
module Compiler.RunicParser
    (
    ) where

import Compiler.RunicParser.Types ( getToken, RunicKeyword(..), RunicObject )
import Compiler.RunicParser.Internal ( tryPopToken, RunicTopLevel, addToContext )
import Control.Monad.Except ( throwError )
import Data.Text ( Text )

{-|

-}
topLevelStateMachine :: RunicTopLevel ()
topLevelStateMachine = do
    possTok <- tryPopToken
    case possTok of
        Nothing -> return () -- exit the process
        Just tracked -> do
            let tok = getToken tracked
            (name, ctxItem) <- case tok of
                Keep -> parseDomainExpression
                Guess -> parseGuessExpression
                Const -> parseConstExpression
                Function -> parseFunctionExpression
                invalidToken -> throwError $ "expected either 'keep', \
                    \'guess', 'const', or 'fn' keyword to start a \
                    \phrase, but found " ++ show invalidToken
            addToContext name ctxItem
            topLevelStateMachine

{-|

-}
parseDomainExpression :: RunicTopLevel (Text, RunicObject)
parseDomainExpression = 