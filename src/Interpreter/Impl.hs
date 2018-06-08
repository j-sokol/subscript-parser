module Interpreter.Impl where

import SubsAst

-- You might need the following imports
import System.IO()
import Text.Printf()
import Control.Monad()
import qualified Data.Map as Map
import Data.Map(Map)

-- | A value is either an integer, the special constant undefined,
--   true, false, a string, or an array of values.
-- Expressions are evaluated to values.
data Value = IntVal Int
           | UndefinedVal
           | TrueVal | FalseVal
           | StringVal String
           | ArrayVal [Value]
           deriving (Eq, Show)


type Error = String
type Env = Map Ident Value
type Primitive = [Value] -> Either Error Value
type PEnv = Map FunName Primitive
type Context = (Env, PEnv)

initialContext :: Context
initialContext = (Map.empty, initialPEnv)
  where initialPEnv =
          Map.fromList [ ("===", undefined)
                       , ("<", undefined)
                       , ("+", undefined)
                       , ("*", undefined)
                       , ("-", undefined)
                       , ("%", undefined)
                       , ("Array", mkArray)
                       ]

newtype SubsM a = SubsM {runSubsM :: Context -> Either Error (a, Env)}

instance Functor SubsM where
  fmap = undefined

instance Applicative SubsM where
  pure = undefined
  (<*>) = undefined

instance Monad SubsM where
  return x = SubsM  (\(env, _) -> Right (x, env))
  f >>= m = SubsM   ( \(env, pEnv) -> case runSubsM f (env, pEnv) of
                                      Left e -> Left e
                                      Right (a, env') -> case runSubsM (m a)
                                                        (env', pEnv) of
                                                            Left e -> Left e
                                                            Right v -> Right v )
  fail s = SubsM  $ \_ -> Left s


mkArray :: Primitive
mkArray [IntVal n] | n >= 0 = return $ ArrayVal (replicate n UndefinedVal)
mkArray _ = Left "Array() called with wrong number or type of arguments"

modifyEnv :: (Env -> Env) -> SubsM ()
modifyEnv f = SubsM $ \context -> Right ((), f $ fst context)

putVar :: Ident -> Value -> SubsM ()
putVar identName value = modifyEnv (Map.insert identName value)

getVar :: Ident -> SubsM Value
getVar identName = SubsM (\(e, _) -> case Map.lookup identName e of
                                      Just v -> Right (v, e)
                                      Nothing -> Left "unbound variable")

getFunction :: FunName -> SubsM Primitive
getFunction funName = SubsM (\(env, penv) -> case Map.lookup funName penv of
                                          Just p -> Right (p, env)
                                          Nothing -> Left ("unbound function" ++
                                            " used but not defined" ++ funName))

evalExpr :: Expr -> SubsM Value
evalExpr (Number int) = return $ IntVal int
evalExpr (String string) = return $ StringVal string
evalExpr (Array xs) = do
                        vals <- mapM evalExpr xs
                        return $ ArrayVal vals
evalExpr Undefined = return UndefinedVal
evalExpr TrueConst = return TrueVal
evalExpr FalseConst = return FalseVal
evalExpr (Var name)  = getVar name
-- Array comprehensions nested in evalExpr, which allows us to save
-- implementation of multiple functions
-- Array Comprehension "Body"
evalExpr (Compr (ACBody expr)) = evalExpr expr
-- Array Comprehension "For"
evalExpr (Compr (ACFor ident expr arrComp)) = do
                                        iterator <- evalExpr expr
                                        -- to save ident val in higher context
                                        tmpIdent <- SubsM (\(env, _) ->
                                                    case Map.lookup ident env of
                                                        (Just v) -> Right
                                                                   (Just v, env)
                                                        Nothing -> Right
                                                                  (Nothing, env)
                                                      )
                                        -- evaluates
                                        res <- evalArrComprehensionACFor
                                                  ident iterator arrComp

                                      -- put value back into context  or delete
                                        case tmpIdent of
                                         (Just v) -> putVar ident v
                                         Nothing -> modifyEnv (Map.delete ident)
                                        return res
-- Array comprehension "If"
evalExpr (Compr (ACIf expr comp)) = do
  trueOrFalse <- evalExpr expr
  if trueOrFalse == TrueVal
    then evalExpr (Compr comp)
    else if trueOrFalse == FalseVal
      then return (ArrayVal[])
      else SubsM (const (Left"ACIf must be accompanied with boolean condition"))
-- evalExpr to call a function name and get result based on function
evalExpr (Call functName params) = do
                                  (ArrayVal values) <- evalExpr (Array params)
                                  function <- getFunction functName
                                  case function values of
                                    Left e -> fail e
                                    Right val -> return val
evalExpr (Assign name expr) = do
                                val <- evalExpr expr
                                putVar name val >> return val
evalExpr (Comma expr1 expr2) = evalExpr expr1 >> evalExpr expr2

-- Function for itterating over array or string for nested lists
evalArrComprehensionACFor :: Ident -> Value -> ArrayCompr -> SubsM Value
-- if array to be iterated over is empty
evalArrComprehensionACFor _ (ArrayVal []) _ = return $ ArrayVal []
evalArrComprehensionACFor ident (StringVal str) arrCompr =
  evalArrComprehensionACFor ident (ArrayVal [StringVal [a] | a <- str]) arrCompr
evalArrComprehensionACFor ident (ArrayVal (x:xs)) arrCompr = do
      putVar ident x
      val <- evalExpr (Compr arrCompr)
      ArrayVal vals <- evalArrComprehensionACFor ident (ArrayVal xs) arrCompr
      case val of
        ArrayVal val' -> return $ ArrayVal (val' ++ vals)
        _ -> return $ ArrayVal (val:vals)
evalArrComprehensionACFor _ _ _ = error "Failed iteratoration,bad type/argument"

runExpr :: Expr -> Either Error Value
runExpr expression = case runSubsM (evalExpr expression) initialContext of
                  Left err -> Left err
                  Right (valOfExpr, _) -> Right valOfExpr
