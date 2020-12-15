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

freeVars :: Expr -> [Symb] 
freeVars (Var a)    = [a]
freeVars (a :@ b)   = (freeVars a) `union` (freeVars b)
freeVars (Lam a e)  = filter (/= a) (freeVars e)

freeTVars :: Type -> [Symb]
freeTVars (TVar a)  = [a]
freeTVars (a :-> b) = (freeTVars a) `union` (freeTVars b)

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env e) a t = Env (e ++ [(a, t)])

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env e) = foldr union [] $ map (freeTVars . snd) e
