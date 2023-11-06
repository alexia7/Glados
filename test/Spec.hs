-- -- TestMyFunctions.hs
import Test.HUnit
import Expression
import Lib
import AST

-- Définition des tests unitaires
testIsOperator :: Test
testIsOperator = TestCase $ do
    assertBool "Test d'un opérateur valide" (isOperator "+")
    assertBool "Test d'un autre opérateur valide" (isOperator "div")
    assertBool "Test d'un opérateur invalide" (not (isOperator "foo"))
    assertBool "Test d'une chaîne vide" (not (isOperator ""))
    assertBool "Test d'une chaîne avec espace" (not (isOperator " "))
    assertBool "Test d'un opérateur avec des espaces" (not (isOperator " + "))

testEitherToMaybe :: IO ()
testEitherToMaybe = do
    putStrLn "Testing eitherToMaybe:"
    let rightValue = Right (42 :: Integer)
    let leftValue = Left "Error message"
    putStrLn $ "Right value: " ++ show (eitherToMaybe rightValue)
    putStrLn $ "Left value: " ++ show (eitherToMaybe leftValue :: Maybe String)

-- Définition des tests unitaires
testParseToAST :: Test
testParseToAST = TestList
    [ "Test de conversion de nombre" ~:
      parseToAST (Number 42) ~?= Right (AstNumber 42)
    , "Test de conversion de nombre à virgule" ~:
      parseToAST (Float 3.14) ~?= Right (AstFloat 3.14)
    , "Test de conversion de booléen" ~:
      parseToAST (Boolean True) ~?= Right (AstBool True)
    , "Test de conversion de chaîne de caractères" ~:
      parseToAST (String "hello") ~?= Right (AstString "hello")
    , "Test de conversion de symbole" ~:
      parseToAST (Symbol "x") ~?= Right (AstSymbol "x")
    , "Test de conversion de liste de types" ~:
      parseToAST (TypeList (Number 1) [Boolean True, Symbol "x"]) ~?=
      Right (AstTypedList (AstNumber 1) [AstBool True, AstSymbol "x"])
    , "Test de conversion de liste" ~:
      parseToAST (List [Number 1, Symbol "add", Number 2]) ~?=
      Right (Array [AstNumber 1, AstSymbol "add", AstNumber 2])
    ]

testParseOperator :: Test
testParseOperator = TestList
    [ "Test de conversion d'un operateur unaire" ~:
      parseOperator "negate" [Number 5] ~?= Right (UnOp "negate" (AstNumber 5))
    , "Test de conversion d'un operateur binaire" ~:
      parseOperator "+" [Number 2, Number 3] ~?= Right (BinOp "+" (AstNumber 2) (AstNumber 3))
    , "Test de liste vide" ~:
      parseOperator "invalid" [] ~?= Left "Invalid operator: empty list"
    , "Test de liste avec trois elements" ~:
      parseOperator "invalid" [Number 1, Number 2, Number 3] ~?= Right (InfOp "invalid" (Array [AstNumber 1, AstNumber 2, AstNumber 3]))
    ]

testParseIf :: Test
testParseIf = TestList
    [ "Test de liste vide" ~:
      parseIf [] ~?= Left "Invalid if"
    , "Test de liste avec un seul élément" ~:
      parseIf [Number 42] ~?= Left "Invalid if"
    ]

testParseArray :: Test
testParseArray = TestList
    [ "Test de liste vide" ~:
      parseArray [] ~?= Right (Array [])
    , "Test de parseIf" ~:
      parseArray [Symbol "if", Number 5, Number 10, Number 20] ~?=
      Right (If (AstNumber 5) (AstNumber 10) (AstNumber 20))
    , "Test de parseDefine" ~:
      parseArray [Symbol "define", Symbol "x", Number 42] ~?=
      Right (Define "x" (AstNumber 42))
    , "Test de parseOperator" ~:
      parseArray [Symbol "+", Number 2, Number 3] ~?=
      Right (BinOp "+" (AstNumber 2) (AstNumber 3))
    , "Test de createArray" ~:
      parseArray [Number 1, Symbol "add", Number 2, Symbol "sub", Number 3] ~?=
      Right (Array [AstNumber 1, AstSymbol "add", AstNumber 2, AstSymbol "sub", AstNumber 3])
    ]

testParseDefine :: Test
testParseDefine = TestList
    [ "Test de définition simple" ~:
      parseDefine [Symbol "x", Number 42] ~?= Right (Define "x" (AstNumber 42))
    , "Test de liste vide" ~:
      parseDefine [] ~?= Left "Invalid define"
    , "Test de liste invalide" ~:
      parseDefine [Symbol "x"] ~?= Left "Invalid define",
      "Test de définition de variable simple" ~:
      parseDefine [Symbol "x", Number 42] ~?= Right (Define "x" (AstNumber 42))
    ,"Test de define avec nom de fonction et expression valide" ~:
      parseDefine [Symbol "myVar", Number 123] ~?=
      Right (Define "myVar" (AstNumber 123))
    , "Test de définition invalide" ~:
      parseDefine [Symbol "invalid"] ~?= Left "Invalid define"
    ]

testParseLambda :: Test
testParseLambda = TestList
    [ "Test de lambda avec une seule expression" ~:
      parseLambda [List [], Number 42] ~?=
      Right (Lambda [] (AstNumber 42))
    , "Test de lambda avec une liste d'arguments invalide" ~:
      parseLambda [Symbol "x", Number 2] ~?= Left "Invalid lambda"
    , "Test de lambda avec expression invalide" ~:
      parseLambda [Symbol "x", Number 2] ~?= Left "Invalid lambda"
    ]

testParseArgList :: Test
testParseArgList = TestList
    [ "Test de parseArgList avec une liste d'arguments vide" ~:
      parseArgList [] ~?= Right []
    , "Test de parseArgList avec une liste d'arguments vide" ~:
      parseArgList [] ~?= Right []
    , "Test de parseArgList avec une liste d'arguments vide" ~:
      parseArgList [] ~?= Right []
    ]

testParseWhile :: Test
testParseWhile = TestList
    [ "Test de parseWhile avec une instruction while invalide" ~:
      parseWhile [Symbol "<", Symbol "x", Number 10, Symbol "+", Symbol "x"] ~?= Left "Invalid while"
    , "Test de while avec condition et deux expressions" ~:
      parseWhile [Number 1, Number 2, Number 3] ~?=
      Right (While (AstNumber 1) [AstNumber 2, AstNumber 3])
    ]

testParseFor :: Test
testParseFor = TestList
    [ "Test de for avec initialisation, condition, incrémentation et expressions valides" ~:
      parseFor [Number 1, Number 2, Number 3, List [Number 4, Number 5]] ~?=
      Right (For (AstNumber 1) (AstNumber 2) (AstNumber 3) [AstNumber 4, AstNumber 5])
    , "Test de for avec une seule expression" ~:
      parseFor [Number 1, Number 2, Number 3, Number 4] ~?= Left "Invalid for"
    , "Test de for avec une liste d'expressions invalide" ~:
      parseFor [Number 1, Number 2, Number 3, Symbol "x"] ~?= Left "Invalid for"
    ]


testValidateExtension :: Test
testValidateExtension = TestList
    [ testExtensionMatch
    , testExtensionMismatch
    , testEmptyExtension
    ]
  where
    testExtensionMatch :: Test
    testExtensionMatch = TestCase $ do
        let ext = "txt"
        let filename = "example.txt"
        assertBool "Extension should match" (validateExtension ext filename)
    testExtensionMismatch :: Test
    testExtensionMismatch = TestCase $ do
        let ext = "txt"
        let filename = "example.jpg"
        assertBool "Extension should not match" (not $ validateExtension ext filename)
    testEmptyExtension :: Test
    testEmptyExtension = TestCase $ do
        let ext = ""
        let filename = "example.txt"
        assertBool "Empty extension should not match" (not $ validateExtension ext filename)


testGetFileExtension :: Test
testGetFileExtension = TestList
    [ testWithExtension
    , testEmptyString
    ]

  where
    testWithExtension :: Test
    testWithExtension = TestCase $ do
        let filename = "example.txt"
        assertEqual "Extension should be 'txt'" "txt" (getFileExtension filename)
    testEmptyString :: Test
    testEmptyString = TestCase $ do
        let filename = ""
        assertEqual "No extension should be returned for empty string" "" (getFileExtension filename)


testGetBetweenParenthesis :: Test
testGetBetweenParenthesis = TestList
    [ testValidInput
    , testMissingOpenParenthesis
    , testMissingCloseParenthesis
    , testEmptyInput
    ]

  where
    testValidInput :: Test
    testValidInput = TestCase $ do
        let input = "(content)"
        assertEqual "Content between parentheses should be 'content'" (Right ("content", "")) (getBetweenParenthesis input 0)
    testMissingOpenParenthesis :: Test
    testMissingOpenParenthesis = TestCase $ do
        let input = "content)"
        assertEqual "Missing open parenthesis should result in an error" (Left "Error: Missing parenthesis") (getBetweenParenthesis input 0)
    testMissingCloseParenthesis :: Test
    testMissingCloseParenthesis = TestCase $ do
        let input = "(content"
        assertEqual "Missing close parenthesis should result in an error" (Left "Error: Missing parenthesis") (getBetweenParenthesis input 0)
    testEmptyInput :: Test
    testEmptyInput = TestCase $ do
        let input = ""
        assertEqual "Empty input should result in an error" (Left "Error: Missing parenthesis") (getBetweenParenthesis input 0)


testGetOnlyValue :: Test
testGetOnlyValue = TestList
    [ testValidInput
    , testEmptyValue
    , testEmptyInput
    ]

  where
    testValidInput :: Test
    testValidInput = TestCase $ do
        let input = "(value)"
        assertEqual "Value between parentheses should be ['value']" (Right (["value"], "")) (getOnlyValue input)
    testEmptyValue :: Test
    testEmptyValue = TestCase $ do
        let input = "()"  -- Valeur vide entre parenthèses
        assertEqual "Empty value should result in an error" (Right ([], "")) (getOnlyValue input)
    testEmptyInput :: Test
    testEmptyInput = TestCase $ do
        let input = ""
        assertEqual "Empty input should result in an error" (Left "Error: Empty value") (getOnlyValue input)


testChangeExtension :: Test
testChangeExtension = TestList
    [ testValidExtension
    , testNoExtension
    , testEmptyInput
    ]

  where
    testValidExtension :: Test
    testValidExtension = TestCase $ do
        let input = "file.txt"
        assertEqual "Extension should be changed to '.go'" "file.go" (changeExtension input)
    testNoExtension :: Test
    testNoExtension = TestCase $ do
        let input = "file"
        assertEqual "No extension should result in the same input" "file.go" (changeExtension input)
    testEmptyInput :: Test
    testEmptyInput = TestCase $ do
        let input = ""
        assertEqual "Empty input should result in an empty output" "" (changeExtension input)


main :: IO ()
main = do
    _ <- runTestTT $ TestList [testIsOperator]
    _ <- runTestTT $ TestList [testParseToAST]
    _ <- runTestTT $ TestList [testParseOperator]
    _ <- runTestTT $ TestList [testParseIf]
    _ <- runTestTT $ TestList [testParseArray]
    _ <- runTestTT $ TestList [testParseDefine]
    _ <- runTestTT $ TestList [testParseLambda]
    _ <- runTestTT $ TestList [testParseArgList]
    _ <- runTestTT $ TestList [testParseWhile]
    _ <- runTestTT $ TestList [testParseFor]
    _ <- runTestTT $ TestList [testValidateExtension]
    _ <- runTestTT $ TestList [testGetFileExtension]
    _ <- runTestTT $ TestList [testGetBetweenParenthesis]
    _ <- runTestTT $ TestList [testGetOnlyValue]
    _ <- runTestTT $ TestList [testChangeExtension]
    testEitherToMaybe
    return ()
