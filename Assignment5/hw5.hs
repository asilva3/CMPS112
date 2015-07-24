--Assignment #5
--Author : Armando Silva asilva3
--Partner: Kevin Thai kjthai
--Time programming together : 6
--Additional individual effort : 2

-- Necessary imports
import Prelude hiding (lookup)
import Data.Maybe
import Control.Applicative ((<$>),liftA,liftA2)
import Data.Map
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import qualified Text.Parsec.Token as P


--------- AST Nodes ---------

-- Variables are identified by their name as string
type Variable = String

-- Values are either integers or booleans
data Value = IntVal Int       -- Integer value
           | BoolVal Bool     -- Boolean value

-- Expressions are variables, literal values, unary and binary operations
data Expression = Var Variable                    -- e.g. x
                | Val Value                       -- e.g. 2
                | BinOp Op Expression Expression  -- e.g. x + 3
                | Assignment Variable Expression  -- e.g. x = 3

-- Statements are expressions, conditionals, while loops and sequences
data Statement = Expr Expression                    -- e.g. x = 23
               | If Expression Statement Statement  -- if e then s1 else s2 end
               | While Expression Statement         -- while e do s end
               | Sequence Statement Statement       -- s1; s2
               | For Variable Expression Expression Statement -- for var in e1 to e2 do s end
               | Skip                               -- no-op

-- All binary operations
data Op = Plus         --  +  :: Int -> Int -> Int
        | Minus        --  -  :: Int -> Int -> Int
        | Times        --  *  :: Int -> Int -> Int
        | GreaterThan  --  >  :: Int -> Int -> Bool
        | Equals       --  == :: Int -> Int -> Bool
        | LessThan     --  <  :: Int -> Int -> Bool

-- The `Store` is an associative map from `Variable` to `Value` representing the memory
type Store = Map Variable Value

--------- Parser ---------

-- The Lexer

lexer = P.makeTokenParser (emptyDef {
  P.identStart = letter,
  P.identLetter = alphaNum,
  P.reservedOpNames = ["+", "-", "*", "!", ">", "=", "==", "<"],
  P.reservedNames = ["true", "false", "if", "in", "then", "else", "while", "end", "to", "do", "for"]
})

-- The Parser

-- Number literals
numberParser :: Parser Value
numberParser = (IntVal . fromIntegral) <$> P.natural lexer

--Variable Parser
varParser :: Parser Variable
varParser = P.identifier lexer

-- Boolean literals
boolParser :: Parser Value
boolParser =  (P.reserved lexer "true" >> return (BoolVal True))
          <|> (P.reserved lexer "false" >> return (BoolVal False))

-- Literals and Variables
valueParser :: Parser Expression
valueParser =  Val <$> (numberParser <|> boolParser)
           <|> Var <$> P.identifier lexer

-- -- Expressions
exprParser :: Parser Expression
exprParser = liftA2 Assignment
                    (try (P.identifier lexer >>= (\v ->
                          P.reservedOp lexer "=" >> return v)))
                    exprParser
          <|> buildExpressionParser table valueParser
    where table = [[Infix (op "*" (BinOp Times)) AssocLeft]
                  ,[Infix (op "+" (BinOp Plus)) AssocLeft]
                  ,[Infix (op "-" (BinOp Minus)) AssocLeft]
                  ,[Infix (op ">" (BinOp GreaterThan)) AssocLeft]
                  ,[Infix (op "==" (BinOp Equals)) AssocLeft]
                  ,[Infix (op "<" (BinOp LessThan)) AssocLeft]]
          op name node = (P.reservedOp lexer name) >> return node

-- Sequence of statements
stmtParser :: Parser Statement
stmtParser = stmtParser1 `chainl1` (P.semi lexer >> return Sequence)

-- Single statements
stmtParser1 :: Parser Statement
stmtParser1 = (Expr <$> exprParser)
          <|> do
              P.reserved lexer "if"
              cond <- exprParser
              P.reserved lexer "then"
              the <- stmtParser
              P.reserved lexer "else"
              els <- stmtParser
              P.reserved lexer "end"
              return (If cond the els)
          <|> do
              P.reserved lexer "while"
              cond <- exprParser
              P.reserved lexer "do"
              body <- stmtParser
              P.reserved lexer "end"
              return (While cond body)
          <|> do
              P.reserved lexer "for"
              var <- varParser
              P.reserved lexer "in"
              expr1 <- exprParser
              P.reserved lexer "to"
              expr2 <- exprParser
              P.reserved lexer "do"
              statement <- stmtParser
              P.reserved lexer "end"
              return (For var expr1 expr2 statement)

-------- Helper functions --------

-- Lift primitive operations on IntVal and BoolVal values
liftIII :: (Int -> Int -> Int) -> Value -> Value -> Value
liftIII f (IntVal x) (IntVal y) = IntVal $ f x y
liftIIB :: (Int -> Int -> Bool) -> Value -> Value -> Value
liftIIB f (IntVal x) (IntVal y) = BoolVal $ f x y

-- Apply the correct primitive operator for the given Op value
applyOp :: Op -> Value -> Value -> Value
applyOp Plus        = liftIII (+)
applyOp Minus       = liftIII (-)
applyOp Times       = liftIII (*)
applyOp GreaterThan = liftIIB (>)
applyOp Equals      = liftIIB (==)
applyOp LessThan    = liftIIB (<)

-- Parse and print (pp) the given WHILE programs
pp :: String -> IO ()
pp input = case (parse stmtParser "" input) of
    Left err -> print err
    Right x  -> print x

-- Parse and run the given WHILE programs
run :: (Show v) => (Parser n) -> String -> (n -> Store -> v) -> IO ()
run parser input eval = case (parse parser "" input) of
    Left err -> print err
    Right x  -> print (eval x empty)

--  Uncomment the following function for question #5 and #6

-- Parse and run the given WHILE programs using monads
runMonad :: String -> Maybe Store
runMonad input = proc (parse stmtParser "" input)
    where proc (Right x) = snd `fmap` runImperative (evalS_monad x) empty
          proc _         = Nothing




-- # 1
instance Show Value where
  show (IntVal x) = show x
  show (BoolVal x) = show x

instance Show Op where
  show (Plus) = "+"
  show (Minus) = "-"
  show (Times) = "*"
  show (GreaterThan) = ">"
  show (Equals) = "="
  show (LessThan) = "<"

instance Show Expression where
  show (Var x) = x
  show (Val x) = show x
  show (BinOp x y z) = show y ++ " " ++ show x ++ " " ++ show z
  show (Assignment x y) = x ++ " = " ++ show y

instance Show Statement where
  show (Expr x) = show x
  show (If x y z) = "if " ++ show x ++ " then " ++ show y ++ " else " ++ show z ++ " end "
  show (While x y) = "while " ++ show x ++ " do " ++ show y ++ " end "
  show (Sequence x y) = show x ++ ";" ++ show y
  show (For x y z z') = "for " ++ x ++ " in " ++ show y ++ " to " ++ show z ++ " do " ++ show z' ++ " end" 
  show (Skip) = ""
  

-- #2 evalE
evalE :: Expression -> Store -> (Value,Store)
evalE (BinOp o a b) s = (applyOp o a' b', s'')
   where (a', s')  = evalE a s
         (b', s'') = evalE b s'
		 
evalE (Var x) s = case lookup x s of
                Just v -> (v,s)
                Nothing -> error "Error"
evalE (Val v) s = (v,s)

evalE (Assignment x e) s = (x', z') where
    x' = fst(evalE e s)
    y' = Data.Map.insert x x' s
    z' = snd(evalE e y')
    
    
-- #3 evalS   
evalS :: Statement -> Store -> Store
evalS w@(While e s1) s = case (evalE e s) of
                          (BoolVal True,s')  -> let s'' = evalS s1 s' in evalS w s''
                          (BoolVal False,s') -> s'
                          _                  -> error "Condition must be a BoolVal"
                          
evalS Skip s             = s
evalS (Expr e) s         = snd(evalE e s)
evalS (Sequence s1 s2) s = evalS s2(evalS s1 s)
evalS (If e s1 s2) s     = case evalE e s of
        (BoolVal True, s')  -> evalS s1 s'
        (BoolVal False, s') -> evalS s2 s'
        _                   -> error "Condition must be a BoolVal"

evalS (For x e1 e2 s1) s = z' where
                      x' = evalE(Assignment x e1) s
                      w' = BinOp Plus e2 (Val(IntVal 1))
                      y' = BinOp GreaterThan w' (Var x) 
                      x'' = Sequence s1 (Expr(Assignment x (BinOp Plus (Var x)  (Val(IntVal 1))))) 
                      z' = evalS(While y' x'') (snd x')
 
-- #4 evalE_maybe

evalE_maybe :: Expression -> Store -> Maybe (Value, Store)
evalE_maybe (BinOp o a b) s = do (a',s') <- evalE_maybe a s
                                 (b',s'') <- evalE_maybe b s'
                                 return (applyOp o a' b', s'')
evalE_maybe (Var x) s = case lookup x s of
                Just v -> Just(v,s)
                Nothing -> Nothing
evalE_maybe (Val v) s = Just (v,s)

evalE_maybe (Assignment x e) s = case evalE_maybe e s of
                                    Just (v,s) -> Just(v, Data.Map.insert x v s)
                                    Nothing -> Nothing
    
    
evalS_maybe :: Statement -> Store -> Maybe Store
evalS_maybe w@(While e s1) s = case (evalE_maybe e s) of
                          Just(BoolVal True,s')  -> let s'' = evalS s1 s' in Just (evalS w s'')
                          Just(BoolVal False,s') -> Just s'
                          _                  -> Nothing
evalS_maybe Skip s             = Just s
evalS_maybe (Sequence s1 s2) s = case (evalS_maybe s1 s) of
                                 Just v -> evalS_maybe s2 v
                                 Nothing -> Nothing
                                 
evalS_maybe (Expr e) s         = case (evalE_maybe e s) of
                                 Just v -> Just(snd (v))
                                 Nothing -> Nothing
                                 
evalS_maybe (If e s1 s2) s     = case (evalE_maybe e s) of
        Just(BoolVal True, s')  -> evalS_maybe s1 s'
        Just(BoolVal False, s') -> evalS_maybe s2 s'
        _                   -> Nothing

evalS_maybe (For x e1 e2 s1) s = z' where
                     x' = case evalE_maybe(Assignment x e1) s of
                          Just (v,s') -> s'
                         -- Nothing -> Nothing
                          
                     w' = BinOp Plus e2 (Val(IntVal 1))
                     y' = BinOp GreaterThan w' (Var x) 
                     x'' = Sequence s1 (Expr(Assignment x (BinOp Plus (Var x)  (Val(IntVal 1)))))
                     z' = evalS_maybe(While y' x'') x'
 
-- # 5 eval_monad       
newtype Imperative a = Imperative {
    runImperative :: Store -> Maybe (a, Store)
}

instance Monad Imperative where
    return a = Imperative (\s -> Just (a,s))
    b >>= f = Imperative (\s -> do (v1,s1) <- (runImperative b) s
                                   runImperative (f v1) s1)
    fail _ = Imperative (\s -> Nothing)        
evalE_monad :: Expression -> Imperative Value
evalE_monad (BinOp o a b) = do a' <- evalE_monad a
                               b' <- evalE_monad b
                               return (applyOp o a' b')

evalE_monad (Var x) = getVar x
evalE_monad (Val v) = return v
evalE_monad (Assignment x e) = do
                               x' <- evalE_monad e
                               y' <- setVar x x'
                               return y'
                               

evalS_monad :: Statement -> Imperative ()
evalS_monad (While e s1)     = do v <- evalE_monad e
                                  case v of
                                    (BoolVal True)   -> do
                                                         evalS_monad s1
                                                         evalS_monad (While e s1)
                                    (BoolVal False)  -> return()
evalS_monad Skip             = return()
evalS_monad (Sequence s1 s2) = do
                                 x' <- evalS_monad s1
                                 y' <- evalS_monad s2
                                 return()
evalS_monad (Expr e)         = do
                               x' <- evalE_monad e
                               return()
evalS_monad (If e s1 s2)     = do v <- evalE_monad e
                                  case v of
                                    (BoolVal True)  -> evalS_monad s1
                                    (BoolVal False) -> evalS_monad s2
                                    _               -> error "Condition must be a BoolVal"
evalS_monad (For x e1 e2 s1) = do
                      x' <- evalE_monad(Assignment x e1) 
                      let w' = BinOp Plus e2 (Val(IntVal 1))
                      let y' = BinOp GreaterThan w' (Var x) 
                      let x'' =  Sequence s1 (Expr(Assignment x (BinOp Plus (Var x)  (Val(IntVal 1))))) 
                      z' <- evalS_monad(While y' x'')
                      return z'


getVar :: Variable -> Imperative Value
getVar var = Imperative (\store -> ((Data.Map.lookup var store) >>= (\v -> Just (v,store))))

setVar :: Variable -> Value -> Imperative Value
setVar var val = Imperative (\store -> Just (val, Data.Map.insert var val store))

miniprog :: Imperative Value
miniprog = do
            setVar "x" (IntVal 2)
            setVar "y" (IntVal 3)
            a <- getVar "x"
            b <- getVar "y"
            return (applyOp Plus a b)