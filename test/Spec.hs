-- TestMyFunctions.hs
import Test.HUnit
import TestFunc

-- Test de la fonction add
testAddition :: Test
testAddition = TestList
  [ TestCase (assertEqual "1 + 1 doit être égal à 2" 2 (add 1 1))
  , TestCase (assertEqual "5 + (-3) doit être égal à 2" 2 (add 5 (-3)))
  , TestCase (assertEqual "0 + 0 doit être égal à 0" 0 (add 0 0))
  ]

-- Suite de tests
main :: IO ()
main = do
  results <- runTestTT (TestList [testAddition])
  putStrLn (show results)