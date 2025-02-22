{-# LANGUAGE Trustworthy #-}
module Data.Runic.Constrainer 
    ( getUnknowns
    , ConstrainT
    ) where

import safe Control.Arrow ((>>>), arr, second)
import safe Control.Monad.Trans (lift)
import safe Control.Monad.Except (throwError, ExceptT)
import safe Control.Monad.Reader (ask, ReaderT)
import safe Control.Monad.State (get, modify, StateT)
import safe Data.List (uncons)
import safe Data.Map (member)
import safe Data.Runic.Types (RnCtx)
import safe Data.Tree (Tree)
import Text.Regex.TDFA ((=~), getAllTextMatches)
import Control.Monad.RWS (modify)

-- DUDE WE NEED TO USE LENSES

-- | The monad transformer used by the Runic problem constrainer
--   it consists of a read-only @RnCtx@ map and a mutable list of 
--   strings to identify which variables have been constrained.
type ConstrainT m = 
    StateT (Tree String, [String]) (ReaderT RnCtx (ExceptT ConstrainError m))

data ConstrainError 
    = EquationPoolEmpty

instance Show ConstrainError where
    show EquationPoolEmpty = "The equation pool was empty."

-- | An alias for @Control.Monad.Except.throwError@ within the 
--   context of a @ConstrainT@ monad transformer stack. 
throwCT :: Monad m => ConstrainError -> ConstrainT m a
throwCT = lift . lift . throwError

getCtx :: Monad m => ConstrainT m RnCtx
getCtx = lift ask

-- | Grabs the equation at the top of the equation pool, leaving
--   it untouched in the process.
headPool :: Monad m => ConstrainT m String
headPool = do
    (_, eqPool) <- get
    case uncons eqPool of
        Just (hd, _) -> return hd
        Nothing -> throwCT EquationPoolEmpty

-- | Moves the equation at the top of the pool to the bottom.
cycle :: Monad m => ConstrainT m ()
cycle = modify $ second cycle'
    where
        cycle' xs = case uncons xs of
            Just (hd, tl) -> tl ++ [hd]
            Nothing -> []

-- | Attempts to constrain an equation, adding it to the dependency tree 
--   if the equation has at most one unknown variable
-- tryConstrain :: Monad m => ConstrainT m ()
-- tryConstrain = do
--     eqn <- headPool
--     ctx <- getCtx
--     case ctx `getUnknowns` eqn of
--         [x] -> 
        
--     return ()


identifierRegex :: String
identifierRegex = "[a-zA-Z_][a-zA-Z0-9_]*"

getVariables :: String -> [String]
getVariables = getAllTextMatches . (=~ identifierRegex)

getUnknowns :: RnCtx -> String -> [String]
getUnknowns ctx = filter (not . (`member` ctx)) . getVariables 

