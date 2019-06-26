module InterpTest
where

import SubsAst
import SubsInterpreter
import Test.HUnit

-- Variables not declared cannot be assigned
assignOne :: Bool
assignOne = if runProg (Prog [ExprAsStm (Assign "i" (Number 23459855))]) ==
			   Left VariableNotDeclared
			then True
			else False

assignTwo :: Bool
assignTwo = if runProg (Prog [ExprAsStm (Assign "yu"(Array [Number 0, Number 1, Number 2,Number 3,Number 4, Number 5,
			   Number 6, Number 7, Number 8, Number 9]))]) ==
			   Left VariableNotDeclared
			then True
			else False

runP :: Program -> Assertion
runP prog = case runProg prog of
        Right _ -> return()
        Left _ -> assertFailure $ show False

-- when a variable is declared it returns the Updated Env
declareOne :: Assertion
declareOne = runP (Prog [VarDecl "x" (Just (Number 42))]) 
--result => Right (fromList [("x",IntVal 42)])


declareTwo :: Assertion
declareTwo = runP (Prog [VarDecl "x" (Just (Array 
				[Number 0, Number 1, Number 2,Number 3]))])
-- result => Right (fromList [("x",ArrayVal [IntVal 0,IntVal 1,IntVal 2,IntVal 3])])

-- Variables can be redeclared and will shadow earlier declarations.
reDeclareOne :: Assertion
reDeclareOne = runP (Prog[VarDecl "squares" (Just (Number 2)), 
				  ExprAsStm (Assign "squares" (Array [Number 4, Number 5]))]) 
-- result => Right (fromList [("squares",ArrayVal [IntVal 4,IntVal 5])])

reDeclareTwo :: Assertion
reDeclareTwo = runP (Prog[VarDecl "squares" (Just (Number 2)), 
				  (VarDecl "squares" (Just(Array [Number 4, Number 5])))])
-- result => Right (fromList [("squares",ArrayVal [IntVal 4,IntVal 5])])

--- add Operator
addOne :: Assertion
addOne = runP (Prog [VarDecl "vare" (Just(Call "+" [Number 1, String "cd"]))])
-- result => Right (fromList [("vare",StringVal "1cd")])

addTwo :: Assertion
addTwo = runP (Prog [VarDecl "vare" (Just(Call "+" [Number 1, Number 8]))])
-- result => Right (fromList [("vare",IntVal 9)])

addThree :: Assertion
addThree = runP (Prog [VarDecl "vare" (Just(Call "+" [String "1a", Number 8]))])
-- result => Right (fromList [("vare",StringVal "1a8")])

addFour :: Assertion
addFour = runP (Prog [VarDecl "vare" (Just(Call "+" [String "1a", String "the"]))])
-- result => Right (fromList [("vare",StringVal "1athe")])

-- less Operator
lessOne :: Assertion
lessOne = runP (Prog [VarDecl "vare" (Just(Call "<" [Number 1, Number 8]))])
-- result => Right (fromList [("vare",TrueVal)])

lessTwo :: Assertion
lessTwo = runP (Prog [VarDecl "vare" (Just(Call "<" [Number 1, String "cd"]))])
-- result => Left MismatchedTypes

lessThree :: Assertion
lessThree = runP (Prog [VarDecl "vare" (Just(Call "<" [String "1a", Number 8]))])
-- result => Left MismatchedTypes

lessFour :: Assertion
lessFour = runP (Prog [VarDecl "vare" (Just(Call "<" [String "1a", String "the"]))])
-- result => Right (fromList [("vare",TrueVal)])

-- modulus Operator
modOne :: Assertion
modOne = runP (Prog [VarDecl "vare" (Just(Call "%" [Number 1, Number 8]))])
-- result => Right (fromList [("vare",IntVal 1)])

modTwo :: Assertion
modTwo = runP (Prog [VarDecl "vare" (Just(Call "%" [Number 1, String "cd"]))])
-- result => Left MismatchedTypes

modThree :: Assertion
modThree = runP (Prog [VarDecl "vare" (Just(Call "%" [String "1a", Number 8]))])
-- result => Left MismatchedTypes

modFour :: Assertion
modFour = runP (Prog [VarDecl "vare" (Just(Call "%" [String "1a", String "the"]))])
-- result => Left NumericValuesOnlyForModulus

-- equality === Operator
equalOne :: Assertion
equalOne = runP (Prog [VarDecl "vare" (Just(Call "===" [Number 1, Number 8]))])
-- result => Right (fromList [("vare",FalseVal)])

equalTwo :: Assertion
equalTwo = runP (Prog [VarDecl "vare" (Just(Call "===" [Number 1, String "cd"]))])
-- result => Left MismatchedTypes

equalThree :: Assertion
equalThree = runP (Prog [VarDecl "vare" (Just(Call "===" [String "1a", Number 8]))])
-- result => Left MismatchedTypes

equalFour :: Assertion
equalFour = runP (Prog [VarDecl "vare" (Just(Call "===" [String "1a", String "the"]))])
-- result => Right (fromList [("vare",FalseVal)])

-- subtract operator
subOne :: Assertion
subOne = runP (Prog [VarDecl "vare" (Just(Call "-" [Number 1, Number 8]))])
-- result => Right (fromList [("vare",IntVal (-7))])

subTwo :: Assertion
subTwo = runP (Prog [VarDecl "vare" (Just(Call "-" [Number 1, String "cd"]))])
-- result => Left MismatchedTypes

subThree :: Assertion
subThree = runP (Prog [VarDecl "vare" (Just(Call "-" [String "1a", Number 8]))])
-- result => Left MismatchedTypes

subFour :: Assertion
subFour = runP (Prog [VarDecl "vare" (Just(Call "-" [String "1a", String "the"]))])
-- result => Left NumericValuesOnlyForSubtract

-- array comprehension : works in a similar way for all operators
arrComp1 :: Assertion
arrComp1 = runP (Prog[VarDecl "squares" (Just (Compr ("x",(Array [Number 0, Number 1, 
				Number 2,Number 3, Number 4, Number 5]), Nothing)
				(Call "*" [Var "x",Var "x"])))])

-- result => Right (fromList [("squares",ArrayVal 
--	[IntVal 0,IntVal 1,IntVal 4,IntVal 9,IntVal 16,IntVal 25])])
arrComp2 :: Assertion
arrComp2 = runP (Prog[VarDecl "squares" (Just (Compr ("x",(Array [Number 0, Number 1, 
				Number 2,Number 3, Number 4, Number 5]), Nothing)
				(Call "%" [Var "x",Var "x"])))])
-- result => Left DivisionByZero

arrComp3 :: Assertion
arrComp3 = runP (Prog[VarDecl "squares" (Just (Compr ("x",(Array [String "laugh", 
				String "ing", String "out"]), Nothing)(Call "+" [Var "x",Var "x"])))])

-- result => Right (fromList [("squares",ArrayVal [StringVal "laughlaugh",StringVal "inging",
--		StringVal "outout"])])

arrComp4 :: Assertion
arrComp4 = runP (Prog[VarDecl "squares" (Just (Compr ("x",(Array [Number 0, Number 1, Number 2,
				Number 3, Number 4, Number 5]), Nothing)(Call "-" [Var "x",Number 4])))])

-- result => Right (fromList [("squares",ArrayVal [IntVal 0,IntVal 0,IntVal 0,
--	IntVal 0,IntVal 0,IntVal 0])])

-- limitation of array comprehension
-- does not work
{-notarrComp1 :: Assertion
notarrComp1 = runP (Prog[VarDecl "squares" (Just (Compr ("x",(Array [String "laugh", 
					String "ing", String "out"]), Nothing)(Call "+" [Var "x","w"])))])-}

-- Multiple For statements do no work
{-VarDecl "evens" (Just (Compr ("x",Var "xs",Just (ArrayIf (Call "===" 
	[Call "%" [Var "x",Number 2], Number 0]) Nothing))
	(Var "x"))),
VarDecl "many_a" (Just (Compr ("x",Var "xs",Just (ArrayForCompr ("y",Var "xs", Nothing)))
	(String "a"))),
VarDecl "hundred" (Just (Compr ("i",Array [Number 0],Just (ArrayForCompr ("x",Var "xs",
	Just (ArrayForCompr ("y",Var "xs", Nothing)))))(Assign "i" (Call "+" [Var "i", Number 1]))))-}