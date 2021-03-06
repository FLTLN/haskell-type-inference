{-# LANGUAGE FlexibleContexts #-}
import Control.Monad.Except
import Data.List

infixl 4 :@
infixr 3 :->

type Symb = String 

-- Терм
data Expr = Var Symb 
          | Expr :@ Expr
          | Lam Symb Expr
  deriving (Eq,Show)

-- Тип
data Type = TVar Symb 
          | Type :-> Type
  deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
  deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
  deriving (Eq,Show)
  
freeTVars :: Type -> [Symb]
freeTVars (TVar a)  = [a]
freeTVars (a :-> b) = (freeTVars a) `union` (freeTVars b)


appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy []) t = t
appSubsTy (SubsTy list) (TVar symbol) = if(fst(head list)) == symbol
                                        then (snd (head list))
                                        else appSubsTy (SubsTy(tail list)) (TVar symbol)
appSubsTy subs (a:-> b) = ((appSubsTy subs a) :-> (appSubsTy subs b))


composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy a b = SubsTy $ sub1 ++ (helper2 a sub1) where
    sub1 = helper1 b a 
    
    helper1 :: SubsTy -> SubsTy -> [(Symb, Type)]
    helper1 (SubsTy a) subs = map (\x -> (fst x, appSubsTy subs (snd x))) a

    helper2 :: SubsTy -> [(Symb, Type)] -> [(Symb, Type)]
    helper2 (SubsTy a) b = filter (\x -> not (isIn b (fst x)) ) a 

    isIn :: [(Symb, Type)] -> Symb -> Bool 
    isIn [] a   = False
    isIn (x:xs) a = if a == fst x then True else isIn xs a
    

unify :: (MonadError String m) => Type -> Type -> m SubsTy
unify (TVar a) (TVar b)
    | a == b = return $ SubsTy []
    | True   = return $ SubsTy [(a , TVar b)]
unify (TVar a) t
    | a `elem` freeTVars t  = throwError $ "Can't unify  ("++(show a)++") with ("++(show t)++")!"
    | otherwise             = return $ SubsTy [(a, t)]
unify (s1 :-> s2) (TVar a)  = unify (TVar a) (s1 :-> s2)
unify (s1 :-> s2) (t1 :-> t2) = do
    let tmp = unify s2 t2
    case tmp of
        Left err -> throwError $ err
        Right s  -> do 
            let tmp' = unify (appSubsTy s s1) (appSubsTy s t1)
            case tmp' of
                Left err -> throwError $ err
                Right s' -> return $ composeSubsTy s' s
