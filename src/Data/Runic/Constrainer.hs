{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE InstanceSigs #-}
module Data.Runic.Constrainer 
    ( ConstrainT
    , ConstrainTState(..)
    , FutureSoln(..)
    ) where

import safe Control.Monad.Except (throwError, ExceptT)
import safe Control.Monad.RWS (asks, gets, RWST)
import safe Control.Monad.Trans (lift)
import safe Data.Set (fromList, difference, intersection, union, Set)
import safe qualified Data.List (union)
import safe Data.Map (member, keys)
import safe Data.Runic.Types (RnCtx)
import Text.Regex.TDFA ((=~), getAllTextMatches)

data ConstrainError 
    = EquationPoolEmpty

instance Show ConstrainError where
    show :: ConstrainError -> String
    show EquationPoolEmpty = "The equation pool was empty."

-- | A record type containing data on which variables are 
--   needed for (and provided by) a solution to a given equation.
data FutureSoln 
    = FutureSoln -- ^ A solution to a one-DOF equation
    { dependencies  :: [String]
    , solnYields    :: String
    , equation      :: String
    }
    | FutureSystem -- ^ A solution to a constrained, N-DOF system
    { dependencies  :: [String]
    , systemYields  :: [String]
    , equations     :: [String]
    }
    deriving (Eq, Ord, Show)

-- | The state stored by a ConstraintT monad action
data ConstrainTState 
    = ConstrainTState 
    { equationPool  :: [String] 
    , learnedValues :: [String]
    }

-- | The monad transformer used by the Runic problem constrainer
--   it consists of a read-only @RnCtx@ map and a mutable list of 
--   strings to identify which variables have been constrained.
type ConstrainT m = RWST 
    RnCtx                       -- ^ The read-only context of knowns and constraints
    [FutureSoln]                -- ^ The equations who have a possible solution
    ConstrainTState             -- ^ The unsolved equation pool 
    (ExceptT ConstrainError m)  -- ^ Adds error-reporting effects

-- | A regex pattern for a Runic-legal identifier.
identifierRegex :: String
identifierRegex = "[a-zA-Z_][a-zA-Z0-9_]*"

-- | Grabs all Runic-legal identifiers from the given equation
--   or expression.
getVariables :: String -> [String]
getVariables = getAllTextMatches . (=~ identifierRegex)

-- | An alias for @Control.Monad.Except.throwError@ within the 
--   context of a @ConstrainT@ monad transformer stack. 
throwError' :: Monad m => ConstrainError -> ConstrainT m a
throwError' = lift . throwError

-- | Grabs a list of all variables currently known by the
--   future solution to the given system of equations. 
knowns :: Monad m => ConstrainT m [String]
knowns = do
    known <- asks keys
    learned <- gets learnedValues
    return $ Data.List.union known learned

-- | Given an equation as a String, returns a list of the known 
--   variables that the equation needs in order to yield a solution as
--   well as a list of the variables that solving this equation may provide.
--   
--   If the second list contains only one variable, then this equation is
--   properly constrained as a one-unknown problem that can be solved to yield 
--   that variable.
getKnownsAndUnknowns :: Monad m => String -> ConstrainT m (Set String, Set String)
getKnownsAndUnknowns eqn = do
    let vars = fromList $ getVariables eqn
    ctxKnowns <- fromList <$> knowns
    return 
        ( intersection vars ctxKnowns   -- The known variables this solution needs
        , difference vars ctxKnowns     -- The variables this solution may provide
        )

