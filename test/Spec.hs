import Test.HUnit ( assertEqual, Counts, runTestTT, test, (~:) )

import Compiler.Parser ( (<?->), (<?+>) )

-- | 
data Token
    = Keep
    | On
    | LBrack
    | RBrack
    | Expr Text
    deriving (Eq, Show)

main :: IO Counts
main = do 
    runTestTT $ test 
        [ "testFnRegex" ~: assertEqual $ liftParser Keep <?-> (Expr "") <?-> On <?-> LBrack <?-> (Expr "") <?-> RBrack
            [ 
            , "fn circle_circumf(r) = TWO_PI * r      \n"
            ]
            (getFnDefinitions exampleFileContents) 
        ]
