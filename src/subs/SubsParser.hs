module SubsParser (
  ParseError(..),
  parseString,
  parseFile
) where


import SubsAst
import Data.Char 
import Text.ParserCombinators.ReadP
import Control.Applicative ((<*), Alternative((<|>)))

data ParseError =  InvalidParse
                 | AmbigousParse
                 deriving (Show, Eq) 

-------------------------------LOW LEVEL FUNCTIONS----------------------------------------------
--a list of reserved words in the language subscript
reserved :: [String]
reserved = ["var", "true", "false", "undefined", "for", "of", "or", "if"]

-- pass some value and skip spaces
token :: ReadP a -> ReadP a
token p = skipSpaces >> p

-- tokenize a symbol
symbol :: String -> ReadP String
symbol = token . string

-- pass a character and remove the spaces
schar :: Char -> ReadP Char
schar str = skipSpaces >> char str

-- pass some string and remove spaces
sstring :: String -> ReadP String
sstring str = skipSpaces >> string str

-- a string should be between single quotes in ht lanugage subscript
cString :: ReadP String
cString = token (
             do 
             _ <- symbol "'"
             cs <- munch isAlpha
             _ <- symbol "'"
             return cs)

-- a valid identifier can start with underscore or any character apart from
-- other symbols like #, &, ^, $
ident :: ReadP Ident
ident  = token (
            do 
            c <- satisfy (\pUnder -> pUnder == '_' || isAlpha pUnder)
            cIdent <- munch (\pUnder -> pUnder == '_' || isAlphaNum pUnder)
            let iden = c:cIdent
            if iden `elem` reserved then pfail else return iden)

-- returns a character that is a digit
digit :: ReadP Char
digit = satisfy isDigit

-- a valid number should not be more than 8 digits long
number :: ReadP Int
number  = token (do
             _ <- schar '-' 
             x <- munch1 isDigit
             let x2 = take 8 x
             return $ negate (read x2))
       <|> token (do 
             x <- munch1 isDigit
             let x2 = take 8 x
             return (read x2))

-------------------------------------GRAMMAR FOR SUBSCRIPT-------------------------------------------
-- exprN, expr', exprC are helper functions used in removing left 
-- recursion and ambiguity

-- Expr -> Expr' ExprN
expr :: ReadP Expr
expr = do 
         exps <-expr'
         exprN exps 

-- ExprN -> ‘,’ Expr’ ExprN | empty
exprN :: Expr -> ReadP Expr
exprN inval = do 
                 _ <- schar ','
                 exps <- expr'
                 exprN (Comma inval exps)
           <|>   return inval

--Expr' -> Expr1 
expr' :: ReadP Expr
expr' = expr1

-- Exprs is a list of expressions, it can be empty
-- Exprs -> empty | Expr1 CommaExprs
exprs :: ReadP [Expr]
exprs = do 
           exps <- expr1
           commaExprs exps
     <|>   return []

-- '='' is put at the top level because of precedence
-- Expr1 -> Ident '=' ExprC | ExprC
expr1 :: ReadP Expr
expr1 = do
           iden <- ident
           _ <- schar '='
           exps <- exprC
           return $ Assign iden exps
     <|>   exprC

-- ExprC -> Expr2
exprC :: ReadP Expr
exprC = expr2

-- '=' Expr1 was taken out to maintain the precedence of the 
--  equality operator
--  AfterIdent -> empty | ’=’ Expr1 | FunCall
afterIdent :: Ident -> ReadP Expr
afterIdent idval =  do funCall idval
                <|> do return $ Var idval

-- FunCall -> ’.’ Ident FunCall | ’(’ Exprs ’)’ | empty
funCall :: Ident -> ReadP Expr
funCall inval = do
                    _ <- schar '.'
                    iden <- ident
                    funCall (inval ++ "." ++ iden)
             <|> do _ <- schar '('
                    exps <- exprs
                    _ <- schar ')'
                    return $ Call inval exps

-- Expr2 -> Expr3 Expr2New
expr2 :: ReadP Expr
expr2 = do 
           exps <- expr3
           expr2New exps

-- '===' Precedence 2
-- Expr2New -> ‘===’ Expr3 Expr2New | empty
expr2New :: Expr -> ReadP Expr
expr2New inval = do 
                    _ <- symbol "==="
                    exps <- expr3
                    expr2New (Call "===" [inval, exps]) 
              <|>   return inval

-- Expr3 -> Expr4 Expr3New
expr3 :: ReadP Expr
expr3 = do 
           exps <- expr4
           expr3New exps

-- '<' Precedence 3
-- Expr3New -> ‘<’ Expr4 Expr3New | empty
expr3New :: Expr -> ReadP Expr
expr3New inval = do 
                    _ <- schar '<'
                    exps <- expr4
                    expr3New (Call "<" [inval, exps])
              <|>   return inval

-- Expr4 -> Expr5 Expr4New
expr4 :: ReadP Expr
expr4 = do 
           exps <- expr5
           expr4New exps


-- '+', '-' Precedence 4
-- Expr4New -> ‘+’ Expr5 Expr4New | ‘-’ Expr5 Expr4New | empty
expr4New :: Expr -> ReadP Expr
expr4New inval = do 
                    _ <- schar '+'
                    exps <- expr5 
                    expr4New (Call "+" [inval, exps])
              <|> do 
                    _ <- schar '-'
                    exps2 <- expr5
                    expr4New (Call "-" [inval, exps2])
              <|>   return inval

-- Remove Monadic value from result in Expr6
-- Expr5 -> Expr6 Expr5New
expr5 :: ReadP Expr
expr5 = do 
           exps <- expr6
           case exps of
                Just a -> expr5New a
                Nothing -> pfail

-- '*', '%' Precedence 5
-- Expr4New -> ‘*’ Expr5 Expr4New | ‘%’ Expr5 Expr4New | empty
expr5New :: Expr -> ReadP Expr
expr5New inval = do 
                    _ <- schar '*'
                    exps1 <- expr6
                    case exps1 of
                        Just a -> expr5New (Call "*" [inval, a])
                        Nothing -> pfail
              <|> do _ <- schar '%'
                     exps2 <- expr6
                     case exps2 of
                        Just b -> expr5New (Call "%" [inval, b])
                        Nothing -> pfail
              <|>    return inval

-- this is the lowest level of the expression, failure more 
-- likely to occur here hence the use of maybe monad
expr6 :: ReadP (Maybe Expr)
expr6 = do n <- number
           return $ Just $ Number n
     <|> do st <- cString
            return $ Just $ String st 
     <|> do _ <- sstring "true"
            return $ Just TrueConst
     <|> do _ <- sstring "false"
            return $ Just FalseConst
     <|> do _ <- sstring "undefined"
            return $ Just Undefined
     <|> do _ <- schar '['
            exps <- exprs
            _ <- schar ']'
            return $ Just $ Array exps
     <|> do iden <- ident
            res <- afterIdent iden
            return $ Just res
     <|> do _ <- schar '['
            _ <- sstring "for"
            _ <- schar '('
            iden <- ident
            _ <- sstring "of"
            exps <- expr1
            _ <- schar ')'
            arrycomp <- arrayCompr
            exps2 <- expr1
            _ <- schar ']'
            return $ Just $ Compr (iden, exps, arrycomp) exps2
     <|> do _ <- schar '('
            exps <- expr
            _ <- schar ')'
            return $ Just exps

-- CommaExprs -> empty |  `,` Expr1 CommaExprs
commaExprs :: Expr -> ReadP [Expr]
commaExprs inval = do
                       _ <- schar ','
                       exps <- expr1
                       exps2 <- commaExprs inval
                       return $ exps : exps2                   
                <|>    return [inval]

-- can be empty hence the use of Maybe monad
-- AssignOpt -> empty |  `=` Expr
assignOpt :: ReadP (Maybe Expr)
assignOpt = do 
               _ <- schar '='
               exps <- expr1
               return $ Just exps
         <|>   return Nothing

-- can be empty hence the use of Maybe monad
-- ArrayCompr -> empty |  
-- `if` `(` Expr `)` ArrayCompr |  `for` `(` Ident `of` Expr `)` ArrayCompr
arrayCompr :: ReadP (Maybe ArrayCompr)
arrayCompr = 
                do 
                _ <- symbol "if"
                _ <- schar '('
                exps <- expr1
                _ <- schar ')'
                res <- arrayCompr
                return $ Just $ ArrayIf exps res
          <|> 
                do
                _ <- symbol "for"
                _ <- schar '(' 
                iden <- ident
                _ <- symbol "of"
                exps2 <- expr1 
                _ <- schar ')'
                res <- arrayCompr
                return $ Just $ ArrayForCompr (iden, exps2, res)
          <|>   return Nothing

-- Stm  -> var Ident AssignOpt |  Expr
stm :: ReadP Stm
stm = do 
         _ <- sstring "var"
         iden <- ident
         res <- assignOpt
         return $ VarDecl iden res
  <|> do exps <- expr
         return $ ExprAsStm exps

-- Every valid statement in subscript ends in a semicolon
-- stms is a list of statements (stm); can be empty
-- Stms ::= ϵ |  Stm `;` Stms
stms :: ReadP [Stm]
stms = do 
          stmt <- stm
          _ <- schar ';'
          remStmt <- stms
          return $ stmt : remStmt
    <|>   return []

--------------------------------TOP- LEVEL FUNCTIONS------------------------------

-- list of statements (stms)
program :: ReadP Program
program = do 
             stmts <- stms 
             return $ Prog stmts

--parseString takes a string and returns either a program or an error
parseString :: String -> Either ParseError Program
parseString s =  
  case readP_to_S (program <* token eof) s of
       [(e, _)] -> Right e
       (_:_)    -> Left AmbigousParse --if parse is ambiguous; more than one result
       []       -> Left InvalidParse --if parse is empty

parseFile :: FilePath -> IO (Either ParseError Program)
parseFile path = fmap parseString $ readFile path
