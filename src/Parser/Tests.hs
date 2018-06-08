module Parser.Tests where

import Parser.Impl

-- import shit
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck as QC

import Control.Applicative (Alternative((<|>), empty, many, some))
import SubsAst



-- Your white-box tests here

tests :: TestTree
tests = testGroup "Tests" [unitTests]

main :: IO ()
main = defaultMain tests


unitTests :: TestTree
unitTests = testGroup "Unit Tests"
    [       testCase "parseString 1" $
            parseString "x = 1+2+3" @?=
              (Right (Assign "x" (Call "+" [Call "+" [Number 1,Number 2],Number 3]))),

            testCase "parseString 2" $
            parseString "xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]" @?=
              (Right (Assign "xs" (Array [Number 0,Number 1,Number 2,Number 3,Number 4,Number 5,Number 6,Number 7,Number 8,Number 9]))),

            testCase "parseString 3" $
            parseString "y     = 4+2,\nx = (1 + 1), [x,y]" @?=
              (Right (Comma (Assign "y" (Call "+" [Number 4,Number 2])) (Comma (Assign "x" (Call "+" [Number 1,Number 1])) (Array [Var "x",Var "y"])))),

              testCase "parseString intro-1 hundred" $
              parseString "xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],\nhundred = [ for (i of [0])\nfor (x of Array(5))\nfor (y of Array(20)) i = i + 1 ],\n[hundred]" @?=
                (Right (Comma (Assign "xs" (Array [Number 0,Number 1,Number 2,Number 3,Number 4,Number 5,Number 6,Number 7,Number 8,Number 9])) (Comma (Assign "hundred" (Compr (ACFor "i" (Array [Number 0]) (ACFor "x" (Call "Array" [Number 5]) (ACFor "y" (Call "Array" [Number 20]) (ACBody (Assign "i" (Call "+" [Var "i",Number 1])))))))) (Array [Var "hundred"])))),


            testCase "parseString intro-2 squares" $
            parseString "xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],\nsquares = [ for (x of xs) x * x ],[squares]" @?=
              (Right (Comma (Assign "xs" (Array [Number 0,Number 1,Number 2,Number 3,Number 4,Number 5,Number 6,Number 7,Number 8,Number 9])) (Comma (Assign "squares" (Compr (ACFor "x" (Var "xs") (ACBody (Call "*" [Var "x",Var "x"]))))) (Array [Var "squares"])))),


              testCase "parseString intro-3 evens" $
            parseString "xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],\nevens = [ for (x of xs) if (x % 2 === 0) x ],\n[evens]" @?=
              (Right (Comma (Assign "xs" (Array [Number 0,Number 1,Number 2,Number 3,Number 4,Number 5,Number 6,Number 7,Number 8,Number 9])) (Comma (Assign "evens" (Compr (ACFor "x" (Var "xs") (ACIf (Call "===" [Call "%" [Var "x",Number 2],Number 0]) (ACBody (Var "x")))))) (Array [Var "evens"])))),
              testCase "parseString intro-4 many_a" $
            parseString "xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9],\nmany_a = [ for (x of xs) for (y of xs) 'a' ],\n[many_a]" @?=
              (Right (Comma (Assign "xs" (Array [Number 0,Number 1,Number 2,Number 3,Number 4,Number 5,Number 6,Number 7,Number 8,Number 9])) (Comma (Assign "many_a" (Compr (ACFor "x" (Var "xs") (ACFor "y" (Var "xs") (ACBody (String "a")))))) (Array [Var "many_a"])))),


          testCase "parseString scope.js" $
        parseString "x = 42,y = [for (x of 'abc') x],[x, y]" @?=
          Right (Comma (Assign "x" (Number 42)) (Comma (Assign "y" (Compr (ACFor "x" (String "abc") (ACBody (Var "x"))))) (Array [Var "x",Var "y"]))),

              testCase "parseString '<' L assoc" $
            parseString "x < y < z" @?=
              Right (Call "<" [Call "<" [Var "x",Var "y"],Var "z"]),
            testCase "parseString '===' L assoc" $
          parseString "x === y === z" @?=
            Right (Call "===" [Call "===" [Var "x",Var "y"],Var "z"]),

            testCase "parseString '%' assoc" $
          parseString "x % y % z" @?=
            Right (Call "%" [Call "%" [Var "x",Var "y"],Var "z"]),

            testCase "parseString '+' assoc" $
          parseString "x + y + z" @?=
            Right (Call "+" [Call "+" [Var "x",Var "y"],Var "z"]),

            testCase "parseString '=' and '===' precedence" $
          parseString "x=1===1" @?=
            Right (Assign "x" (Call "===" [Number 1,Number 1])),


            testCase "parseString '+' and '===' precedence " $
          parseString "x+1===2" @?=
            Right (Call "===" [Call "+" [Var "x",Number 1],Number 2]),


            testCase "parseString '-' assoc" $
          parseString "x - y - z" @?=
            Right (Call "-" [Call "-" [Var "x",Var "y"],Var "z"]),

          -- those are not working
          --  testCase "parseString comment" $
          --parseString "x =5 // kunda" @?=
          --  Right (Assign "x" (Number 5)),
          
             testCase "parseString string" $
           parseString "'testing'" @?=
             Right (String "testing"),
          
           --  testCase "parseString string" $
           --parseString "'testing\nkek'" @?=
           --  Right (String "testingkek"),


            testCase "parseString 4" $
            parseString "x = (1 + 1), y     = 4+2, [x,y]" @?=
              (Right (Comma (Assign "x" (Call "+" [Number 1,Number 1])) (Comma (Assign "y" (Call "+" [Number 4,Number 2])) (Array [Var "x",Var "y"])))),

              --- more tests

               testCase "parseString WS trimming" $
             parseString " x     = 23" @?=
               Right (Assign "x" (Number 23)),




               testCase "parseString String backslash" $
             parseString "'testing\\\\'" @?=
               Right (String "testing\\"),

             testCase "parseString String apostrophe" $
             parseString "'testing\\''" @?=
               Right (String "testing'"),

             testCase "parseString String tabulator" $
             parseString "'testing\\t'" @?=
               Right (String "testing\t")


             --  testCase "parseString for variable" $
             --parseString " for     = 23" @?=
             --  Left _

    ]
-- also some
