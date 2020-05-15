module Ruse.Eval where

import Ruse.Syntax

--
--
-- Big-step evaluator
--
-- Given an RExpr, evaluate it to either Right of an RExpr representing the
-- final value, or Left of a String representing a runtime error. Use the error
-- strings that have been provided; you shouldn't need any other runtime errors.
--
-- You should not try to detect if the original RExpr represents a
-- non-terminating program---your evaluator should simply loop in that case.
--
-- When evaluating an RExpr with multiple components (for instance, `Plus`), you
-- should try to evaluate the components in order. If any component returns an
-- error (Left _), your evaluator should return this error (rather than changing
-- the error). Note, however, that some constructs are lazy (And, Or, If, Cond).
-- The big-step semantics document describes how these things should be
-- evaluated.
--
--

eval :: RExpr -> Either String RExpr
eval e = case e of
           NumC i -> Right $ NumC i
           BoolC b -> Right $ BoolC b
           StrC s -> Right $ StrC s
           Plus e1 e2 -> evalPlus e1 e2
           Subt e1 e2 -> evalSubt e1 e2
           Mult e1 e2 -> evalMult e1 e2
           Ifte e e1 e2 -> evalIfte e e1 e2
           And e1 e2 -> evalAnd e1 e2
           Or e1 e2 -> evalOr e1 e2
           Not e -> evalNot e
           IsEq e1 e2 -> evalIsEq e1 e2
           IsLt e1 e2 -> evalIsLt e1 e2
           IsGt e1 e2 -> evalIsGt e1 e2
           IsNil e -> evalIsNil e
           List es -> evalList es
           Cons e es -> evalCons e es
           Car e -> evalCar e
           Cdr e -> evalCdr e
           Var v -> Right $ Var v
           Lam f -> Right $ Lam f
           App e1 e2 -> evalApp e1 e2
           Rec e -> evalRec e
           Cond es e -> evalCond es e
          --  _ -> undefined

-- We've done Plus for you, but the code is very ugly. Start by cleaning up this
-- function using do-notation. You should be able to eliminate the first two
-- case analyses.
evalPlus :: RExpr -> RExpr -> Either String RExpr
-- evalPlus e1 e2 =
--   case eval e1 of
--     Left s1 -> Left s1
--     Right e1' ->
--       case eval e2 of
--         Left s2 -> Left s2
--         Right e2' ->
--           case (e1', e2') of
--             (NumC i1, NumC i2) -> return $ NumC $ i1 + i2
--             _ -> Left "Add on non-numeric"
evalPlus e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ NumC $ i1 + i2
                      _ -> Left "Add on non-numeric"

evalSubt :: RExpr -> RExpr -> Either String RExpr
-- evalSubt e1 e2 = Left "Sub on non-numeric"
evalSubt e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ NumC $ i1 - i2
                      _ -> Left "Sub on non-numeric"

evalMult :: RExpr -> RExpr -> Either String RExpr
-- evalMult e1 e2 = Left "Mul on non-numeric"
evalMult e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ NumC $ i1 * i2
                      _ -> Left "Mul on non-numeric"

evalIfte :: RExpr -> RExpr -> RExpr -> Either String RExpr
-- evalIfte e e1 e2 = Left "If-then-else guard not Boolean"
evalIfte e e1 e2 = do e <- eval e
                      e1' <- eval e1
                      e2' <- eval e2
                      case (e ,e1', e2') of
                        (BoolC True, NumC i1, _) -> return $ NumC i1
                        (BoolC False, _, NumC i2) -> return $ NumC i2
                        _ -> Left "If-then-else guard not Boolean"

evalAnd :: RExpr -> RExpr -> Either String RExpr
-- evalAnd e1 e2 = Left "And on non-Boolean"
evalAnd e1 e2 = do e1' <- eval e1
                   e2' <- eval e2
                   case (e1', e2') of
                    (BoolC True, BoolC b1) -> return $ BoolC b1
                    (BoolC False, _) -> return $ BoolC False
                    _ -> Left "And on non-Boolean"

evalOr :: RExpr -> RExpr -> Either String RExpr
-- evalOr e1 e2 = Left "Or on non-Boolean"
evalOr e1 e2 = do e1' <- eval e1
                  e2' <- eval e2
                  case (e1', e2') of
                    (BoolC True, _) -> return $ BoolC True
                    (BoolC False, BoolC b2) -> return $ BoolC b2
                    _ -> Left "Or on non-Boolean"

evalNot :: RExpr -> Either String RExpr
-- evalNot e = Left "Not on non-Boolean"
evalNot e = do e' <- eval e
               case e' of
                 BoolC True -> return $ BoolC False
                 BoolC False -> return $ BoolC True
                 _ -> Left "Not on non-Boolean"

evalIsEq :: RExpr -> RExpr -> Either String RExpr
-- evalIsEq e1 e2 = Left "Equality on non-numeric"
evalIsEq e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ BoolC (i1 == i2)
                      _ -> Left "Equality on non-numeric"

evalIsLt :: RExpr -> RExpr -> Either String RExpr
-- evalIsLt e1 e2 = Left "Lt on non-numeric"
evalIsLt e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ BoolC (i1 < i2)
                      _ -> Left "Lt on non-numeric"

evalIsGt :: RExpr -> RExpr -> Either String RExpr
-- evalIsGt e1 e2 = Left "Gt on non-numeric"
evalIsGt e1 e2 = do e1' <- eval e1
                    e2' <- eval e2
                    case (e1', e2') of
                      (NumC i1, NumC i2) -> return $ BoolC (i1 > i2)
                      _ -> Left "Gt on non-numeric"

evalIsNil :: RExpr -> Either String RExpr
-- evalIsNil e = Left "IsNil on non-list"
evalIsNil e = do e' <- eval e
                 case e' of
                   List [] -> return $ BoolC True
                   List _ -> return $ BoolC False
                   _ -> Left "IsNil on non-list"

evalList :: [RExpr] -> Either String RExpr
-- evalList es = undefined
evalList [] = return $ List []
evalList (x:xs) = do x' <- eval x
                     xs' <- evalList xs
                     case (x', xs') of
                       (x', List []) -> return $ List [x']
                       (x', List ys') -> return $ List (x':ys')
                       _ -> Left "List on non-list"


evalCons :: RExpr -> RExpr -> Either String RExpr
-- evalCons e es = Left "Cons on non-list"
-- eval (Cons (NumC 12) (List [NumC 13, NumC 14]))
evalCons e es = do e' <- eval e
                   es' <- eval es
                   case (e', es') of
                     (e', List xs) -> return $ List (e':xs)
                     _ -> Left "Cons on non-list"

evalCar :: RExpr -> Either String RExpr
-- evalCar e = Left "Car of empty list"
-- Other error: Left "Car of non-list"
-- evalCar (List [BoolC False, NumC 12, NumC 13])
evalCar e = do e' <- eval e
               case e' of
                 List [] -> Left "Car of empty list"
                 List (x:xs) -> return x
                 _ -> Left "Car of non-list"

evalCdr :: RExpr -> Either String RExpr
-- evalCdr e = Left "Cdr of empty list"
-- Other error: Left "Cdr of non-list"
-- evalCdr (List [BoolC False, NumC 12, NumC 13])
evalCdr e = do e' <- eval e
               case e' of
                 List [] -> Left "Cdr of empty list"
                 List (x:xs) -> return $ List xs
                 _ -> Left "Cdr of non-list"

evalApp :: RExpr -> RExpr -> Either String RExpr
-- evalApp e e' = Left "Apply non-function"
-- Rec (Lam (Plus (Var 1) (NumC 1)))
-- eval (App (Lam (Plus (Var 1) (NumC 1))) (NumC 1))
-- eval (App (Lam (Mult (Var 1) (NumC 42))) (NumC 24))
evalApp e1 e2 = do e1' <- eval e1
                   e2' <- eval e2
                   case (e1', e2') of
                     (Lam f, e2') -> eval (subst e2' f)
                     (_, e2') -> Left "Apply non-function"

evalRec :: RExpr -> Either String RExpr
-- evalRec e = undefined
evalRec e = do e' <- eval e
               case e' of
                 NumC n -> return $ NumC n
                 BoolC b -> return $ BoolC b
                 Rec e'' -> evalRec (subst (Rec e'') e'')
                 _ -> Left "Recursion error"

evalCond :: [(RExpr, RExpr)] -> RExpr -> Either String RExpr
evalCond [] e = eval e
evalCond (x:xs) e = case eval (fst x) of
                         Right (BoolC True) -> eval (snd x)
                         Right (BoolC False) -> evalCond xs e
                         _ -> Left "Invalid cond guard"