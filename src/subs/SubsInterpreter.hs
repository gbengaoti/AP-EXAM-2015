module SubsInterpreter
       ( runProg
       , Error (..)
       , Value(..)
       )
       where

import SubsAst
import Control.Monad
import Control.Applicative((<*>), Applicative(..))
import Data.Map(Map)
import qualified Data.Map as Map

-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)

--  Any runtime error.
data Error = VariableNotFound
           | FunctionNotFound
           | EnvNotUpdated
           | VariableNotDeclared
           | MismatchedTypes
           | NumericValuesOnlyForSubtract
           | NumericValuesOnlyForMultiply
           | NumericValuesOnlyForModulus
           | DivisionByZero
           | CantHandleThis
             deriving (Show, Eq)

type Env = Map Ident Value
type Primitive = [Value] -> SubsM Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

---------------------------------------------LOWER-LEVEL FUNCIONS------------------

-- The initial context initializes the env (variable table) to empty
-- and intializes all operators 
initialContext :: Context 
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", equalsOperator)
                       , ("<", lessOperator)
                       , ("+", addOperator)
                       , ("*", multOperator)
                       , ("-", subOperator)
                       , ("%", modOperator)
                       , ("Array.new", arrayNew)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}
  
instance Functor SubsM where
  fmap = liftM

instance Applicative SubsM where
  pure = return 
  (<*>) = ap

instance Monad SubsM where
  return x = SubsM $ \(env,penv) -> Right (x, env)
  f >>= m  = SubsM $ \(env,penv) -> 
    case runSubsM f (env,penv) of
      Left errorString -> Left errorString
      Right (a, env') -> runSubsM (m a) (env',penv)  

-- For giving eror messages
err :: Error -> SubsM a
err s = SubsM f
      where f _ = Left s

modify :: (Env -> Env) -> SubsM ()
modify f = undefined

-- updateEnv updates the Env when there is a variable declaration
-- or an existing variable is modified
updateEnv :: Ident -> Value -> SubsM ()
updateEnv name val = SubsM $ \(env,penv) -> 
  Right((), Map.insert name val (Map.fromList (Map.toList env)))

-- get the value of a variable using the variable name
getVar :: Ident -> SubsM Value
getVar name = SubsM $ \(env,_) -> 
  case lookup name (Map.toList env) of
    (Just x) -> Right (x,env)
    Nothing ->  Left VariableNotDeclared

-- get a function like "+" from the context; in order to use its
-- definition to perform operations
getFunction :: FunName -> SubsM Primitive
getFunction name = SubsM $ \(env,penv) ->
  case lookup name (Map.toList penv) of 
    Just x -> Right (x,env)
    Nothing -> Left FunctionNotFound

--------------------------------PRIMITIVES DECLARATION------------------------------
-- initialize a new array to contain undefined values
arrayNew :: Primitive
arrayNew [IntVal n] | n > 0 = return $ ArrayVal(replicate n UndefinedVal)
arrayNew _ = fail "Array.new called with wrong number of arguments"

-- the modulo operator can be used on integers only otherwise return an error
modOperator :: Primitive
modOperator [IntVal x, IntVal y] =
  if y == 0 
  then err DivisionByZero 
  else return $ IntVal (x `mod` y)

modOperator [StringVal x, IntVal y] = 
  err MismatchedTypes

modOperator [IntVal x, StringVal y] = 
  err MismatchedTypes

modOperator [StringVal x, StringVal y] = 
  err NumericValuesOnlyForModulus

-- the add operator can be used on both strings and numbers and 
-- a combination of the two which is a concatenation.
addOperator :: Primitive
addOperator [IntVal x, IntVal y] = 
  return $ IntVal (x + y)

addOperator [StringVal x, IntVal y] = 
  return $ StringVal (x ++ show y)

addOperator [IntVal x, StringVal y] = 
  return $ StringVal (show x ++ y)

addOperator [StringVal x, StringVal y] = 
  return $ StringVal (x ++ y)

-- the subtraction operator can be used on integers only otherwise
-- return an error
subOperator :: Primitive
subOperator [IntVal x, IntVal y] = 
  return $ IntVal (x - y)

subOperator [StringVal x, IntVal y] = 
  err MismatchedTypes

subOperator [IntVal x, StringVal y] = 
  err MismatchedTypes

subOperator [StringVal x, StringVal y] =
  err NumericValuesOnlyForSubtract

-- the multiply operator can be used on integers only otherwise 
-- return an error
multOperator :: Primitive
multOperator [IntVal x, IntVal y] = 
  return $ IntVal (x * y)

multOperator [StringVal x, IntVal y] =
  err MismatchedTypes

multOperator [IntVal x, StringVal y] = 
  err MismatchedTypes

multOperator [StringVal x, StringVal y] =
  err NumericValuesOnlyForMultiply

-- the less than operator can be used on strings and integers but 
-- not on the combination of the two
lessOperator :: Primitive
lessOperator [StringVal x, StringVal y] = 
  return (if x < y then TrueVal else FalseVal)

lessOperator [IntVal x, IntVal y] = 
  return (if x < y then TrueVal else FalseVal)

lessOperator [StringVal x, IntVal y] = 
  err MismatchedTypes

lessOperator [IntVal x, StringVal y] = 
  err MismatchedTypes

-- the equals than operator can be used on strings and integers but
-- not on the combination of the two
equalsOperator :: Primitive
equalsOperator [StringVal x, StringVal y] = 
  return (if x == y then TrueVal else FalseVal)

equalsOperator [IntVal x, IntVal y] = 
  return (if x == y then TrueVal else FalseVal)

equalsOperator [StringVal x, IntVal y] = 
  err MismatchedTypes

equalsOperator [IntVal x, StringVal y] = 
  err MismatchedTypes

----------------------------------------EXPRESSIONS----------------------------------

evalExpr :: Expr -> SubsM Value
evalExpr (Number n) =  return $ IntVal n

evalExpr (TrueConst) = return TrueVal

evalExpr (FalseConst) = return FalseVal

evalExpr (Undefined) = return UndefinedVal

evalExpr (String st) = return $ StringVal st

-- getFunction "+" gets the primitive from the context and uses it
-- perform the add operation
evalExpr (Call "+" [aAdd, bAdd]) = do
  aAdd1 <- evalExpr aAdd
  bAdd1 <- evalExpr bAdd
  funcAdd <- getFunction "+"
  funcAdd [aAdd1, bAdd1]

-- getFunction "-" gets the primitive from the context and uses it
-- perform the subtract operation
evalExpr (Call "-" [aMinus, bMinus]) = do
  aMinus1 <- evalExpr aMinus
  bMinus1 <- evalExpr bMinus
  funcSub <- getFunction "-"
  funcSub [aMinus1, bMinus1]

-- getFunction "*" gets the primitive from the context and uses it
-- perform the multiply operation
evalExpr (Call "*" [aMult, bMult]) = do
  aMult1 <- evalExpr aMult
  bMult1 <- evalExpr bMult
  funcMult <- getFunction "*"
  funcMult [aMult1, bMult1]

-- getFunction "%" gets the primitive from the context and uses it
-- perform the modulo operation
evalExpr (Call "%" [aMod, bMod]) = do
  aMod1 <- evalExpr aMod
  bMod1 <- evalExpr bMod
  funcMod <- getFunction "%"
  funcMod [aMod1, bMod1]

-- getFunction "<" gets the primitive from the context and uses it
-- perform the less than operation
evalExpr (Call "<" [aLess, bLess]) = do
  aLess1 <- evalExpr aLess
  bLess1 <- evalExpr bLess
  funcLess <- getFunction "<"
  funcLess [aLess1, bLess1]

-- getFunction "===" gets the primitive from the context and uses it
-- perform the equality operation
evalExpr (Call "===" [aEquals, bEquals]) = do
  aEquals1 <- evalExpr aEquals
  bEquals1 <- evalExpr bEquals
  funcEquals <- getFunction "==="
  funcEquals [aEquals1, bEquals1]

evalExpr (Var name) = 
  return $ StringVal name

evalExpr (Array expList) = do
  res <- mapM evalExpr expList
  return $ ArrayVal res

-- ARRAY COMPREHENSIONS --only special cases
-- [for (i of [ 1, 2, 3 ]) i*i ]; 
-- [ 1, 4, 9 ]
evalExpr(Compr (iden, expr1, marrayComp) expr) = do
{-case marrayComp of 
  Nothing -> do-}
    case expr of 
      (Call p [a,b]) -> do
          v <- evalExpr expr1
          let vals  = calc expr expr1
          ev <- mapM evalExpr vals
          return $ ArrayVal ev
      _ -> err CantHandleThis
     {- (Var remExpr) -> do
          v <- evalExpr expr1
          let vals  = calc expr expr1
          ev <- mapM evalExpr vals
          return $ ArrayVal ev
      (Assign a b) -> do
          v <- evalExpr expr1
          let vals  = calc expr expr1
          ev <- mapM evalExpr vals
          return $ ArrayVal ev-}
 {- (ArrayIf exprIf(Nothing)) -> do
          v <- evalExpr expr1
          let vals  = calc exprIf expr1
          return $ ArrayVal vals-}

-- getVar will return an error, no need for this to do so too
-- after updating, just returning empty here
evalExpr (Assign name e2) = do
  e2Val <- evalExpr e2
  -- check if Variable name is in the Env before updating
  varRes <- getVar name
  updateEnv name e2Val
  return $ StringVal ""

-- performs operations seperated by commmas and returns the result of
-- the last operation
evalExpr (Comma e1 e2) = do
  e1Val <- evalExpr e1
  evalExpr e2        
---------------------------------STATEMENTS----------------------------------
stm :: Stm -> SubsM ()
-- variable declaration here
stm (VarDecl name e1) = 
  case e1 of
    -- updating Env with new variable
    Just a -> do evalE1 <- evalExpr a
                 updateEnv name evalE1
    Nothing -> updateEnv name UndefinedVal

-- execting expressions through stm
stm (ExprAsStm e2) = do
  evalE2 <- evalExpr e2
  return ()

------------------------------TOP-LEVEL FUNCTIONS -------------------------------

-- a list of statements seperated by ;
program :: Program -> SubsM ()
program (Prog prog) = mapM_ stm prog
  
-- run a program, return either an error or the environment
-- which contains variables names and their values 
runProg :: Program -> Either Error Env
runProg prog = 
 case runSubsM (program prog) initialContext of
      Right (_, a) -> Right a
      Left err -> Left err

-----private helper
calc:: Expr -> Expr -> [Expr]

calc (Call p lis) (Array values) = [(Call p [x,x]) | x <- values]

-- FAILED TRIALS
{-calc (Call p lis) (Var xs) = do
  varRes <- getVar xs
  case varRes of
    (ArrayVal xVal)-> [(Call p [x,x]) | x <- xVal]
    _ -> err VariableNotDeclared-}
{-calc (Call p [Var a, Number b]) (Array values) = do
  [(Call p [x,Number b]) | x <- values]-}

{-calc (Call p [Call q [Var a, Number c], Number d]) (Array values) = do
  [x | x <- values, evalExpr(Call q [x, Number c])(Number d == )]-}
{-calc (Var name) (String values) = do
  varRes <- getVar name
  [(Call "+" [x,y]) | x <- values, y<- varRes]-}