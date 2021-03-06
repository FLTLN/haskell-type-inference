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
    
    
instance Monoid SubsTy where
    mappend = composeSubsTy
    mempty  = SubsTy []
