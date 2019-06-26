module ParserTest
where

import SubsParser
import SubsAst

-- returns True : successful parse
-- Declaration of a variable [array]
arrayDeclaration :: Bool
arrayDeclaration = do 
    if parseString "var adjectives = [ 'cold', 'awesome', 'nice' ];" ==
        Right (Prog [VarDecl "adjectives" (Just 
            (Array [String "awesome",String "nice",String "cold"]))])
    then True
    else False

-- does not pass digits of length more than 8
-- truncates digits with length greater than 8
digitCount :: Bool
digitCount = do
    if parseString "i=23459855094090;" == Right (Prog [ExprAsStm (Assign "i" (Number 23459855))])
    then True
    else False

-- parses strings with single quotes
singleQuotes :: Bool
singleQuotes = do 
    if parseString "i = 'p';" == Right (Prog [ExprAsStm (Assign "i" (String "p"))])
    then True
    else False

-- array comprehensions  [for (x of iterable) x]
arrayComp :: Bool
arrayComp = do
    if parseString "[ for (x of xs) 0 ];" == 
        Right (Prog [ExprAsStm (Compr ("x",Var "xs",Nothing) (Number 0))])
    then True
    else False

arrayComp2 :: Bool
arrayComp2 = do
    if parseString "[ for (x of xs) x * -1 ];" == 
        Right (Prog [ExprAsStm (Compr ("x",Var "xs",Nothing) 
            (Call "*" [Var "x",Number (-1)]))])
    then True
    else False

arrayComp3 :: Bool
arrayComp3 = do
    if parseString "[ for (x of xs) 'abd' ];" == 
        Right (Prog [ExprAsStm (Compr ("x",Var "xs",Nothing) (String ""))])
    then True
    else False

arrayComp4 :: Bool
arrayComp4 = do
    if parseString "[ for (x of xs) x*x+2 ];" == 
        Right (Prog [ExprAsStm (Compr ("x",Var "xs",Nothing) 
            (Call "+" [Call "*" [Var "x",Var "x"],Number 2]))])
    then True
    else False
 
-- array comprehensions [for (x of iterable) if (condition) x]
arrayIfCompr :: Bool
arrayIfCompr = do 
    if parseString "var evens = [for (i of numbers) if (i % 2 === 0) i];" ==
        Right (Prog [VarDecl "evens" (Just (Compr ("i",Var "numbers",Just (ArrayIf (Call "===" [Call "%" [Var "i",Number 2],Number 0]) Nothing)) 
            (Var "i")))])
    then True
    else False

arrayIfCompr2 :: Bool
arrayIfCompr2 = do 
    if parseString "var doubledEvens = [for (i of numbers) if (i % 2 === 0) i * 2];" ==
        Right (Prog [VarDecl "doubledEvens" (Just (Compr ("i",Var "numbers",Just (ArrayIf (Call "===" [Call "%" [Var "i",Number 2],Number 0]) Nothing)) 
            (Call "*" [Var "i",Number 2])))])
    then True
    else False

-- no greater than operator in subscript
arrayIfCompr3 :: Bool
arrayIfCompr3 = do
    if parseString "[for (year of years) if (year > 2000) if(year < 2010) year];" ==
        Left InvalidParse
    then True
    else False

arrayIfCompr4 :: Bool
arrayIfCompr4 = do
    if parseString "[for (year of years) if (year < 2000) if(year < 2010) year];" ==
        Right (Prog [ExprAsStm (Compr ("year",Var "years",Just (ArrayIf (Call "<" [Var "year",Number 2000]) 
            (Just (ArrayIf (Call "<" [Var "year",Number 2010]) Nothing)))) (Var "year"))])
    then True
    else False

-- array comprehensions [for (x of iterable) for (y of iterable) x + y]
arrayMultiFor :: Bool
arrayMultiFor = do
    if parseString "var cross = [for (i of numbers) for (j of letters) i+j];" ==
        Right (Prog [VarDecl "cross" (Just (Compr ("i",Var "numbers",Just (ArrayForCompr ("j",Var "letters",Nothing))) 
            (Call "+" [Var "i",Var "j"])))])
    then True
    else False

-- variable names with expressions, expressions with operators, precedence
exprT :: Bool
exprT = do
    if parseString "1*2+3%4;" ==
        Right (Prog [ExprAsStm (Call "+" [Call "*" [Number 1,Number 2],Call "%" [Number 3,Number 4]])])
    then True
    else False

exprT1 :: Bool
exprT1 = do
    if parseString "i = i + 3%4;" ==
        Right (Prog [ExprAsStm (Assign "i" (Call "+" [Var "i",Call "%" [Number 3,Number 4]]))])
    then True
    else False

-- statements must end in semicolon
stmtEnd :: Bool
stmtEnd = do 
    if (parseString "i=23459855094090") == 
        (Left InvalidParse)
    then True
    else False

stmtEnd2 :: Bool    
stmtEnd2 = do
    if (parseString "i=23459855094090;") == 
        (Right (Prog [ExprAsStm (Assign "i" (Number 23459855))]))
    then True
    else False

-- identifiers test
-- identifiers can start with underscores, contain no spaces,
-- cannot be a reserved word
identT :: Bool
identT = do
    if parseString "_deg = 60;" ==
        Right (Prog [ExprAsStm (Assign "_deg" (Number 60))])
    then True
    else False

identT2 :: Bool
identT2 = do
    if parseString "__ = 60;" ==
        Right (Prog [ExprAsStm (Assign "__" (Number 60))])
    then True
    else False

-- contains spaces
identT3 :: Bool
identT3 = do
    if parseString "_ g = 60;" ==
        Left InvalidParse
    then True
    else False

-- reserved word used
identT4 :: Bool
identT4 = do
    if parseString "true = 1;" ==
        Left InvalidParse
    then True
    else False

-- sample programs
programT :: Bool
programT = do
    if parseString "var x = 42;var y = [for (x of 'abc') x];var z = x;" ==
        Right (Prog [VarDecl "x" (Just (Number 42)),
            VarDecl "y" (Just (Compr ("x",String "abc",Nothing) (Var "x"))),
            VarDecl "z" (Just (Var "x"))])
    then True
    else False

-- run simpleProgram using parseString simplePrgramN
simpleProgram1 :: String
simpleProgram1 = "var numbers = [ 1, 2, 3 ];var letters = [ 'a', 'b', 'c' ];\n"++
                  "var cross = [for (i of numbers)for (j of letters) i+j];"

simpleProgram2 :: String
simpleProgram2 = "var grid = [for (i of numbers) [for (j of letters) i+j]];"

simpleProgram3 :: String
simpleProgram3 = "[for (i of numbers) if (i < 1) for (j of letters) if(j < 'a') i+j];"

simpleProgram4 :: String
simpleProgram4 = "[for (i of numbers) for (j of letters) if (i < 1) if(j < 'a') i+j]; "

simpleProgram5 :: String
simpleProgram5=  "[for (i of numbers) if (i < 1) [for (j of letters) if(j < 'a') i+j]];"

simpleProgram6 :: String
simpleProgram6 = "[for (i of numbers) [for (j of letters) if (i < 1) if(j < 'a') i+j]];"

-- result for simplePrograms
{-results :: String
results = "Right (Prog 
   [VarDecl "numbers" (Just (Array [Number 2,Number 3,Number 1])),
    VarDecl "letters" (Just (Array [String "b",String "c",String "a"])),
    VarDecl "cross" (Just (Compr ("i",Var "numbers",Just (ArrayForCompr ("j",Var "letters",
        Nothing))) (Call "+" [Var "i",Var "j"]))),
    VarDecl "grid" (Just (Compr ("i",Var "numbers",Nothing) 
     (Compr ("j",Var "letters",Nothing) (Call "+" [Var "i",Var "j"])))),
    ExprAsStm (Compr ("i",Var "numbers",Just (ArrayIf (Call "<" [Var "i",Number 1]) 
        (Just (ArrayForCompr ("j",Var "letters",Just (ArrayIf (Call "<" [Var "j",String "a"]) 
            Nothing)))))) 
        (Call "+" [Var "i",Var "j"])),
    ExprAsStm (Compr ("i",Var "numbers",Just (ArrayForCompr ("j",Var "letters",
        Just (ArrayIf (Call "<" [Var "i",Number 1]) 
        (Just (ArrayIf (Call "<" [Var "j",String "a"]) Nothing)))))) (Call "+" [Var "i",Var "j"])),
    ExprAsStm (Compr ("i",Var "numbers",Just (ArrayIf (Call "<" [Var "i",Number 1]) Nothing)) 
        (Compr ("j",Var "letters",Just (ArrayIf (Call "<" [Var "j",String "a"]) Nothing)) 
        (Call "+" [Var "i",Var "j"]))),
    ExprAsStm (Compr ("i",Var "numbers",Nothing) (Compr ("j",Var "letters",
        Just (ArrayIf (Call "<" [Var "i",Number 1]) (Just (ArrayIf (Call "<" [Var "j",String "a"]) 
            Nothing)))) 
            (Call "+" [Var "i",Var "j"])))])"-}

-- Given test programs
testProg :: String
testProg = "var xs = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9];\n" ++
           "var squares = [ for (x of xs) x * x ];\n"++
           "var evens = [ for (x of xs) if (x % 2 === 0) x ];\n" ++
           "var many_a = [ for (x of xs) for (y of xs) 'a' ];\n" ++
           "var hundred = [ for (i of [0])\n"++
           "for (x of xs)\n" ++
           "for (y of xs) i = i + 1 ];\n"

-- result
{-Right (Prog 
    [VarDecl "xs" (Just (Array [Number 1,Number 2,Number 3,Number 4,Number 5,
        Number 6,Number 7,Number 8,Number 9,Number 0])),
    VarDecl "squares" (Just (Compr ("x",Var "xs",Nothing) (Call "*" [Var "x",Var "x"]))),
    VarDecl "evens" (Just (Compr ("x",Var "xs",Just (ArrayIf 
        (Call "===" [Call "%" [Var "x",Number 2],Number 0]) Nothing)) (Var "x"))),
    VarDecl "many_a" (Just (Compr ("x",Var "xs",Just (ArrayForCompr ("y",Var "xs",Nothing))) 
        (String "a"))),
    VarDecl "hundred" (Just (Compr ("i",Array [Number 0],
        Just (ArrayForCompr ("x",Var "xs",Just (ArrayForCompr ("y",Var "xs",Nothing))))) 
        (Assign "i" (Call "+" [Var "i",Number 1]))))])-}


