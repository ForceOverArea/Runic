{-# LANGUAGE OverloadedStrings #-}
module Domain 
    ( validateDomainDecl
    , CtxItem
    , RunicItem
    ) where

import Control.Monad.State.Lazy ( evalState, get, put )
import Data.Text ( intercalate, split, toLower, unpack, Text )
import Text.Read ( readMaybe )
import Tokenizer ( RunicItem(..), RunicToken(..) )
import CompilerInternal 
    ( rStateTransition
    , rSyntaxErrorMsg
    , rUnexpectedErrMsg
    , CtxItem(DomainObj)
    , Domain(..)
    , DomainBound(..)
    , RStateTransition 
    )

-- | The Runic compiler substate that is responsible for checking the 
-- validity of a domain declaration
validateDomainDecl :: [RunicItem] -> Either String (CtxItem, [RunicItem])
validateDomainDecl tokens = do
    evalState (s0 tokens) ("", "")
    where
        s0 :: RStateTransition (Text, Text)
        s0 = rStateTransition RKeep (RExpression "")
            (\lineNo tok rs -> case tok of
                (RExpression t) -> do 
                    put (t, "")
                    s1 rs
                _ -> return (Left $ rUnexpectedErrMsg 
                    $ rSyntaxErrorMsg (RExpression "") RKeep (Just $ RunicItem lineNo tok)))

        s1 :: RStateTransition (Text, Text)
        s1 = rStateTransition (RExpression "") ROn (\_ _ rs -> s2 rs)

        s2 :: RStateTransition (Text, Text)
        s2 = rStateTransition RLBracket ROn (\_ _ rs -> s3 rs)

        s3 :: RStateTransition (Text, Text)
        s3 = rStateTransition RLBracket (RExpression "")
            (\lineNo tok rs -> case tok of
                (RExpression t) -> do
                    (e1, _) <- get
                    put (e1, t)
                    s4 rs
                _ -> return (Left $ rUnexpectedErrMsg 
                    $ rSyntaxErrorMsg (RExpression "") RKeep (Just $ RunicItem lineNo tok)))

        s4 :: RStateTransition (Text, Text)
        s4 = rStateTransition (RExpression "") RRBracket
            (\lineNo _ rs -> do 
                nameBounds <- get
                return $ do
                    obj <- readDomain lineNo nameBounds
                    return (obj, rs))

-- | Attempts to create a DomainObj CtxItem from two Text's.
-- If this operation fails, it did so because the expression of the
-- bounds of the domain could not be parsed correctly  
readDomain :: Int -> (Text, Text) -> Either String CtxItem
readDomain lineNo (name, bounds) = do
    domain <- getBounds $ split (== ',') bounds
    return $ DomainObj name domain
    where
        getBounds :: [Text] -> Either String Domain
        getBounds [b1, b2] = do
            b1val <- readBound b1
            b2val <- readBound b2
            return $ Domain b1val b2val
        getBounds t =
            Left $ "failed to parse domain bounds expression: "
            ++ unpack (intercalate "" t)
            ++ " on line "
            ++ show lineNo

        readBound :: Text -> Either String DomainBound
        readBound b
            | "inf" == toLower b = Right Inf
            | "-inf" == toLower b = Right NegInf
            | otherwise =
                case (readMaybe . unpack) b of
                    Just val -> Right $ Bound val
                    Nothing -> Left
                        $ "failed to parse domain bounds expression on line: "
                        ++ show lineNo
