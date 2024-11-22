import Test.HUnit ( assertEqual, Counts, runTestTT, test, TestCase, (~:) )
import Lib 
    ( getConstDefinitions
    , getFnDefinitions 
    )

exampleFileContents :: String
exampleFileContents = 
    "const PI = 22 / 7                      \n\
    \const TWO_PI = PI * 2                  \n\
    \                                       \n\
    \fn circle_area(r) = return PI * r^2    \n\
    \                                       \n\
    \fn circle_circumf(r) = TWO_PI * r      \n\
    \                                       \n"

testConstRegex :: TestCase
testConstRegex = assertEqual "consts in exampleFileContents: " 
    [ "const PI = 22 / 7                      \n"
    , "const TWO_PI = PI * 2                  \n"
    ]
    (getConstDefinitions exampleFileContents)

main :: IO Counts
main = do 
    runTestTT $ test 
        [ "testConstRegex" ~: 
        , "testFnRegex" ~: assertEqual "functions in exampleFileContents: " 
            [ "fn circle_area(r) = return PI * r^2    \n"
            , "fn circle_circumf(r) = TWO_PI * r      \n"
            ]
            (getFnDefinitions exampleFileContents) 
        ]
