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
  

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy []) t = t
appSubsTy (SubsTy list) (TVar symbol) = if(fst(head list)) == symbol
                                        then (snd (head list))
                                        else appSubsTy (SubsTy(tail list)) (TVar symbol)
appSubsTy subs (a:-> b) = ((appSubsTy subs a) :-> (appSubsTy subs b))


appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv s (Env xs) = Env $ map (\x -> (fst x, appSubsTy s (snd x))) xs
