module CompilerInternal
    ( rStateTransition
    , rSyntaxErrorMsg
    , rUnexpectedErrMsg
    , CtxItem(..)
    , Domain(..)
    , DomainBound(..)
    , RStateTransition
    , RunicContext
    ) where

import Control.Monad.State.Lazy ( State )
import Data.Map ( Map )
import Data.Text ( Text )
import Tokenizer ( RunicItem(..), RunicToken(..) )

-- | Alias for a Map String CtxItem that contains all of the data
-- useful to the Runic solver runtime. 
type RunicContext = Map String CtxItem

-- | Alias for the function signature required for and produced by the 
-- rStateTransition function. 
type RStateTransition a = ([RunicItem] -> State a (Either String (CtxItem, [RunicItem])))

data CtxItem
    = ConstObj Text Double
    | FnObj Text (Int -> [Double] -> Double)
    | DomainObj Text Domain
    | GuessObj Text Double

data Domain = Domain DomainBound DomainBound
data DomainBound
    = Inf
    | NegInf
    | Bound Double

rUnexpectedErrMsg :: String -> String
rUnexpectedErrMsg msg = "encountered unexpected error: " ++ msg ++ ". please report this issue at "

rSyntaxErrorMsg :: RunicToken -> RunicToken -> Maybe RunicItem -> String
rSyntaxErrorMsg ex prev fnd = "expected " ++ exString ++ " following " ++ prevString ++ ", found: " ++ fndString
    where 
        exString = show ex
        prevString = show prev
        fndString = show fnd

-- | A factory function that returns a function useful for transitioning 
-- between states in a Runic-parsing Mealy machine.
rStateTransition :: RunicToken -> RunicToken -> (Int -> RunicToken -> RStateTransition a) -> RStateTransition a
rStateTransition prev ex nextState = stateTransitionClosure
    where
        stateTransitionClosure [] = return (Left $ rSyntaxErrorMsg ex prev Nothing)
        stateTransitionClosure (RunicItem lineNo tok:rs)
            | ex `rTokenEq` tok = nextState lineNo tok rs
            | otherwise = return (Left $ rSyntaxErrorMsg ex prev (Just $ RunicItem lineNo tok))

        rTokenEq :: RunicToken -> RunicToken -> Bool
        rTokenEq (RExpression _) (RExpression _) = True
        rTokenEq r1 r2 = r1 == r2
