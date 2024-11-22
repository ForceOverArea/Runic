module Lib
    ( getContext
    ) where

import Data.Text ( Text, pack )
import Data.Map ( empty )
import Text.Regex.TDFA ( getAllTextMatches, (=~) )
import Shunting ( Context )

data ContextDefinition
    = Function String [String] String -- Name Args Expr
    | Const String String -- Name Expr
    | Domain String Double Double -- Name Min Max
    | Guess String Double -- Name Value

legalNamePattern :: String
legalNamePattern = "[a-zA-Z_][a-zA-Z0-9_]*"

legalConstPattern :: String
legalConstPattern = "const +(" ++ legalNamePattern ++ ") *=(.*)\n"

legalFnPattern :: String
legalFnPattern = "fn +(" ++ legalNamePattern ++ ") *\\((" ++ legalArgsPattern ++ ")\\) *=(.*)\n"
    where 
        legalArgsPattern :: String
        legalArgsPattern = "([a-zA-Z_][a-zA-Z0-9_]*,? *)*"

getContext :: Text -> (Text, Context)
getContext fileText = (pack "", empty)
    where
        getFirstConst :: Text -> (String, String, String, [String])
        getFirstConst = (=~) legalConstPattern

        getFirstFunction :: Text -> (String, String, String, [String])
        getFirstFunction = (=~) legalFnPattern