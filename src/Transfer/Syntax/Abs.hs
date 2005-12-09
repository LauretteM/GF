{-# OPTIONS_GHC -fglasgow-exts #-}
module Transfer.Syntax.Abs where

import Control.Monad (ap,MonadPlus,msum,mplus,mzero)
import Data.Monoid

-- Haskell module generated by the BNF converter

data Module_
type Module = Tree Module_
data Import_
type Import = Tree Import_
data Decl_
type Decl = Tree Decl_
data ConsDecl_
type ConsDecl = Tree ConsDecl_
data Guard_
type Guard = Tree Guard_
data Pattern_
type Pattern = Tree Pattern_
data CommaPattern_
type CommaPattern = Tree CommaPattern_
data FieldPattern_
type FieldPattern = Tree FieldPattern_
data Exp_
type Exp = Tree Exp_
data VarOrWild_
type VarOrWild = Tree VarOrWild_
data LetDef_
type LetDef = Tree LetDef_
data Case_
type Case = Tree Case_
data Bind_
type Bind = Tree Bind_
data FieldType_
type FieldType = Tree FieldType_
data FieldValue_
type FieldValue = Tree FieldValue_
data Ident_
type Ident = Tree Ident_

data Tree :: * -> * where
    Module :: [Import] -> [Decl] -> Tree Module_
    Import :: Ident -> Tree Import_
    DataDecl :: Ident -> Exp -> [ConsDecl] -> Tree Decl_
    TypeDecl :: Ident -> Exp -> Tree Decl_
    ValueDecl :: Ident -> [Pattern] -> Guard -> Exp -> Tree Decl_
    DeriveDecl :: Ident -> Ident -> Tree Decl_
    ConsDecl :: Ident -> Exp -> Tree ConsDecl_
    GuardExp :: Exp -> Tree Guard_
    GuardNo :: Tree Guard_
    POr :: Pattern -> Pattern -> Tree Pattern_
    PListCons :: Pattern -> Pattern -> Tree Pattern_
    PConsTop :: Ident -> Pattern -> [Pattern] -> Tree Pattern_
    PCons :: Ident -> [Pattern] -> Tree Pattern_
    PRec :: [FieldPattern] -> Tree Pattern_
    PEmptyList :: Tree Pattern_
    PList :: [CommaPattern] -> Tree Pattern_
    PTuple :: CommaPattern -> [CommaPattern] -> Tree Pattern_
    PType :: Tree Pattern_
    PStr :: String -> Tree Pattern_
    PInt :: Integer -> Tree Pattern_
    PVar :: Ident -> Tree Pattern_
    PWild :: Tree Pattern_
    CommaPattern :: Pattern -> Tree CommaPattern_
    FieldPattern :: Ident -> Pattern -> Tree FieldPattern_
    EPi :: VarOrWild -> Exp -> Exp -> Tree Exp_
    EPiNoVar :: Exp -> Exp -> Tree Exp_
    EAbs :: VarOrWild -> Exp -> Tree Exp_
    ELet :: [LetDef] -> Exp -> Tree Exp_
    ECase :: Exp -> [Case] -> Tree Exp_
    EIf :: Exp -> Exp -> Exp -> Tree Exp_
    EDo :: [Bind] -> Exp -> Tree Exp_
    EBind :: Exp -> Exp -> Tree Exp_
    EBindC :: Exp -> Exp -> Tree Exp_
    EOr :: Exp -> Exp -> Tree Exp_
    EAnd :: Exp -> Exp -> Tree Exp_
    EEq :: Exp -> Exp -> Tree Exp_
    ENe :: Exp -> Exp -> Tree Exp_
    ELt :: Exp -> Exp -> Tree Exp_
    ELe :: Exp -> Exp -> Tree Exp_
    EGt :: Exp -> Exp -> Tree Exp_
    EGe :: Exp -> Exp -> Tree Exp_
    EListCons :: Exp -> Exp -> Tree Exp_
    EAdd :: Exp -> Exp -> Tree Exp_
    ESub :: Exp -> Exp -> Tree Exp_
    EMul :: Exp -> Exp -> Tree Exp_
    EDiv :: Exp -> Exp -> Tree Exp_
    EMod :: Exp -> Exp -> Tree Exp_
    ENeg :: Exp -> Tree Exp_
    EApp :: Exp -> Exp -> Tree Exp_
    EProj :: Exp -> Ident -> Tree Exp_
    ERecType :: [FieldType] -> Tree Exp_
    ERec :: [FieldValue] -> Tree Exp_
    EEmptyList :: Tree Exp_
    EList :: [Exp] -> Tree Exp_
    ETuple :: Exp -> [Exp] -> Tree Exp_
    EVar :: Ident -> Tree Exp_
    EType :: Tree Exp_
    EStr :: String -> Tree Exp_
    EInteger :: Integer -> Tree Exp_
    EDouble :: Double -> Tree Exp_
    EMeta :: Tree Exp_
    VVar :: Ident -> Tree VarOrWild_
    VWild :: Tree VarOrWild_
    LetDef :: Ident -> Exp -> Tree LetDef_
    Case :: Pattern -> Guard -> Exp -> Tree Case_
    BindVar :: VarOrWild -> Exp -> Tree Bind_
    BindNoVar :: Exp -> Tree Bind_
    FieldType :: Ident -> Exp -> Tree FieldType_
    FieldValue :: Ident -> Exp -> Tree FieldValue_
    Ident :: String -> Tree Ident_

composOp :: (forall a. Tree a -> Tree a) -> Tree c -> Tree c
composOp f = head . composOpM (\x -> [f x])

composOpM_ :: Monad m => (forall a. Tree a -> m ()) -> Tree c -> m ()
composOpM_ = composOpFold (return ()) (>>)

composOpMPlus :: MonadPlus m => (forall a. Tree a -> m b) -> Tree c -> m b
composOpMPlus = composOpFold mzero mplus

composOpMonoid :: Monoid m => (forall a. Tree a -> m) -> Tree c -> m
composOpMonoid = composOpFold mempty mappend

composOpM :: Monad m => (forall a. Tree a -> m (Tree a)) -> Tree c -> m (Tree c)
composOpM f t = case t of
    Module imports decls -> return Module `ap` mapM f imports `ap` mapM f decls
    Import i -> return Import `ap` f i
    DataDecl i exp consdecls -> return DataDecl `ap` f i `ap` f exp `ap` mapM f consdecls
    TypeDecl i exp -> return TypeDecl `ap` f i `ap` f exp
    ValueDecl i patterns guard exp -> return ValueDecl `ap` f i `ap` mapM f patterns `ap` f guard `ap` f exp
    DeriveDecl i0 i1 -> return DeriveDecl `ap` f i0 `ap` f i1
    ConsDecl i exp -> return ConsDecl `ap` f i `ap` f exp
    GuardExp exp -> return GuardExp `ap` f exp
    POr pattern0 pattern1 -> return POr `ap` f pattern0 `ap` f pattern1
    PListCons pattern0 pattern1 -> return PListCons `ap` f pattern0 `ap` f pattern1
    PConsTop i pattern patterns -> return PConsTop `ap` f i `ap` f pattern `ap` mapM f patterns
    PCons i patterns -> return PCons `ap` f i `ap` mapM f patterns
    PRec fieldpatterns -> return PRec `ap` mapM f fieldpatterns
    PList commapatterns -> return PList `ap` mapM f commapatterns
    PTuple commapattern commapatterns -> return PTuple `ap` f commapattern `ap` mapM f commapatterns
    PVar i -> return PVar `ap` f i
    CommaPattern pattern -> return CommaPattern `ap` f pattern
    FieldPattern i pattern -> return FieldPattern `ap` f i `ap` f pattern
    EPi varorwild exp0 exp1 -> return EPi `ap` f varorwild `ap` f exp0 `ap` f exp1
    EPiNoVar exp0 exp1 -> return EPiNoVar `ap` f exp0 `ap` f exp1
    EAbs varorwild exp -> return EAbs `ap` f varorwild `ap` f exp
    ELet letdefs exp -> return ELet `ap` mapM f letdefs `ap` f exp
    ECase exp cases -> return ECase `ap` f exp `ap` mapM f cases
    EIf exp0 exp1 exp2 -> return EIf `ap` f exp0 `ap` f exp1 `ap` f exp2
    EDo binds exp -> return EDo `ap` mapM f binds `ap` f exp
    EBind exp0 exp1 -> return EBind `ap` f exp0 `ap` f exp1
    EBindC exp0 exp1 -> return EBindC `ap` f exp0 `ap` f exp1
    EOr exp0 exp1 -> return EOr `ap` f exp0 `ap` f exp1
    EAnd exp0 exp1 -> return EAnd `ap` f exp0 `ap` f exp1
    EEq exp0 exp1 -> return EEq `ap` f exp0 `ap` f exp1
    ENe exp0 exp1 -> return ENe `ap` f exp0 `ap` f exp1
    ELt exp0 exp1 -> return ELt `ap` f exp0 `ap` f exp1
    ELe exp0 exp1 -> return ELe `ap` f exp0 `ap` f exp1
    EGt exp0 exp1 -> return EGt `ap` f exp0 `ap` f exp1
    EGe exp0 exp1 -> return EGe `ap` f exp0 `ap` f exp1
    EListCons exp0 exp1 -> return EListCons `ap` f exp0 `ap` f exp1
    EAdd exp0 exp1 -> return EAdd `ap` f exp0 `ap` f exp1
    ESub exp0 exp1 -> return ESub `ap` f exp0 `ap` f exp1
    EMul exp0 exp1 -> return EMul `ap` f exp0 `ap` f exp1
    EDiv exp0 exp1 -> return EDiv `ap` f exp0 `ap` f exp1
    EMod exp0 exp1 -> return EMod `ap` f exp0 `ap` f exp1
    ENeg exp -> return ENeg `ap` f exp
    EApp exp0 exp1 -> return EApp `ap` f exp0 `ap` f exp1
    EProj exp i -> return EProj `ap` f exp `ap` f i
    ERecType fieldtypes -> return ERecType `ap` mapM f fieldtypes
    ERec fieldvalues -> return ERec `ap` mapM f fieldvalues
    EList exps -> return EList `ap` mapM f exps
    ETuple exp exps -> return ETuple `ap` f exp `ap` mapM f exps
    EVar i -> return EVar `ap` f i
    VVar i -> return VVar `ap` f i
    LetDef i exp -> return LetDef `ap` f i `ap` f exp
    Case pattern guard exp -> return Case `ap` f pattern `ap` f guard `ap` f exp
    BindVar varorwild exp -> return BindVar `ap` f varorwild `ap` f exp
    BindNoVar exp -> return BindNoVar `ap` f exp
    FieldType i exp -> return FieldType `ap` f i `ap` f exp
    FieldValue i exp -> return FieldValue `ap` f i `ap` f exp
    _ -> return t

composOpFold :: b -> (b -> b -> b) -> (forall a. Tree a -> b) -> Tree c -> b
composOpFold zero combine f t = case t of
    Module imports decls -> foldr combine zero (map f imports) `combine` foldr combine zero (map f decls)
    Import i -> f i
    DataDecl i exp consdecls -> f i `combine` f exp `combine` foldr combine zero (map f consdecls)
    TypeDecl i exp -> f i `combine` f exp
    ValueDecl i patterns guard exp -> f i `combine` foldr combine zero (map f patterns) `combine` f guard `combine` f exp
    DeriveDecl i0 i1 -> f i0 `combine` f i1
    ConsDecl i exp -> f i `combine` f exp
    GuardExp exp -> f exp
    POr pattern0 pattern1 -> f pattern0 `combine` f pattern1
    PListCons pattern0 pattern1 -> f pattern0 `combine` f pattern1
    PConsTop i pattern patterns -> f i `combine` f pattern `combine` foldr combine zero (map f patterns)
    PCons i patterns -> f i `combine` foldr combine zero (map f patterns)
    PRec fieldpatterns -> foldr combine zero (map f fieldpatterns)
    PList commapatterns -> foldr combine zero (map f commapatterns)
    PTuple commapattern commapatterns -> f commapattern `combine` foldr combine zero (map f commapatterns)
    PVar i -> f i
    CommaPattern pattern -> f pattern
    FieldPattern i pattern -> f i `combine` f pattern
    EPi varorwild exp0 exp1 -> f varorwild `combine` f exp0 `combine` f exp1
    EPiNoVar exp0 exp1 -> f exp0 `combine` f exp1
    EAbs varorwild exp -> f varorwild `combine` f exp
    ELet letdefs exp -> foldr combine zero (map f letdefs) `combine` f exp
    ECase exp cases -> f exp `combine` foldr combine zero (map f cases)
    EIf exp0 exp1 exp2 -> f exp0 `combine` f exp1 `combine` f exp2
    EDo binds exp -> foldr combine zero (map f binds) `combine` f exp
    EBind exp0 exp1 -> f exp0 `combine` f exp1
    EBindC exp0 exp1 -> f exp0 `combine` f exp1
    EOr exp0 exp1 -> f exp0 `combine` f exp1
    EAnd exp0 exp1 -> f exp0 `combine` f exp1
    EEq exp0 exp1 -> f exp0 `combine` f exp1
    ENe exp0 exp1 -> f exp0 `combine` f exp1
    ELt exp0 exp1 -> f exp0 `combine` f exp1
    ELe exp0 exp1 -> f exp0 `combine` f exp1
    EGt exp0 exp1 -> f exp0 `combine` f exp1
    EGe exp0 exp1 -> f exp0 `combine` f exp1
    EListCons exp0 exp1 -> f exp0 `combine` f exp1
    EAdd exp0 exp1 -> f exp0 `combine` f exp1
    ESub exp0 exp1 -> f exp0 `combine` f exp1
    EMul exp0 exp1 -> f exp0 `combine` f exp1
    EDiv exp0 exp1 -> f exp0 `combine` f exp1
    EMod exp0 exp1 -> f exp0 `combine` f exp1
    ENeg exp -> f exp
    EApp exp0 exp1 -> f exp0 `combine` f exp1
    EProj exp i -> f exp `combine` f i
    ERecType fieldtypes -> foldr combine zero (map f fieldtypes)
    ERec fieldvalues -> foldr combine zero (map f fieldvalues)
    EList exps -> foldr combine zero (map f exps)
    ETuple exp exps -> f exp `combine` foldr combine zero (map f exps)
    EVar i -> f i
    VVar i -> f i
    LetDef i exp -> f i `combine` f exp
    Case pattern guard exp -> f pattern `combine` f guard `combine` f exp
    BindVar varorwild exp -> f varorwild `combine` f exp
    BindNoVar exp -> f exp
    FieldType i exp -> f i `combine` f exp
    FieldValue i exp -> f i `combine` f exp
    _ -> zero

instance Show (Tree c) where
  showsPrec n t = case t of
    Module imports decls -> opar n . showString "Module" . showChar ' ' . showsPrec 1 imports . showChar ' ' . showsPrec 1 decls . cpar n
    Import i -> opar n . showString "Import" . showChar ' ' . showsPrec 1 i . cpar n
    DataDecl i exp consdecls -> opar n . showString "DataDecl" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 exp . showChar ' ' . showsPrec 1 consdecls . cpar n
    TypeDecl i exp -> opar n . showString "TypeDecl" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 exp . cpar n
    ValueDecl i patterns guard exp -> opar n . showString "ValueDecl" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 patterns . showChar ' ' . showsPrec 1 guard . showChar ' ' . showsPrec 1 exp . cpar n
    DeriveDecl i0 i1 -> opar n . showString "DeriveDecl" . showChar ' ' . showsPrec 1 i0 . showChar ' ' . showsPrec 1 i1 . cpar n
    ConsDecl i exp -> opar n . showString "ConsDecl" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 exp . cpar n
    GuardExp exp -> opar n . showString "GuardExp" . showChar ' ' . showsPrec 1 exp . cpar n
    GuardNo -> showString "GuardNo"
    POr pattern0 pattern1 -> opar n . showString "POr" . showChar ' ' . showsPrec 1 pattern0 . showChar ' ' . showsPrec 1 pattern1 . cpar n
    PListCons pattern0 pattern1 -> opar n . showString "PListCons" . showChar ' ' . showsPrec 1 pattern0 . showChar ' ' . showsPrec 1 pattern1 . cpar n
    PConsTop i pattern patterns -> opar n . showString "PConsTop" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 pattern . showChar ' ' . showsPrec 1 patterns . cpar n
    PCons i patterns -> opar n . showString "PCons" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 patterns . cpar n
    PRec fieldpatterns -> opar n . showString "PRec" . showChar ' ' . showsPrec 1 fieldpatterns . cpar n
    PEmptyList -> showString "PEmptyList"
    PList commapatterns -> opar n . showString "PList" . showChar ' ' . showsPrec 1 commapatterns . cpar n
    PTuple commapattern commapatterns -> opar n . showString "PTuple" . showChar ' ' . showsPrec 1 commapattern . showChar ' ' . showsPrec 1 commapatterns . cpar n
    PType -> showString "PType"
    PStr str -> opar n . showString "PStr" . showChar ' ' . showsPrec 1 str . cpar n
    PInt n -> opar n . showString "PInt" . showChar ' ' . showsPrec 1 n . cpar n
    PVar i -> opar n . showString "PVar" . showChar ' ' . showsPrec 1 i . cpar n
    PWild -> showString "PWild"
    CommaPattern pattern -> opar n . showString "CommaPattern" . showChar ' ' . showsPrec 1 pattern . cpar n
    FieldPattern i pattern -> opar n . showString "FieldPattern" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 pattern . cpar n
    EPi varorwild exp0 exp1 -> opar n . showString "EPi" . showChar ' ' . showsPrec 1 varorwild . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EPiNoVar exp0 exp1 -> opar n . showString "EPiNoVar" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EAbs varorwild exp -> opar n . showString "EAbs" . showChar ' ' . showsPrec 1 varorwild . showChar ' ' . showsPrec 1 exp . cpar n
    ELet letdefs exp -> opar n . showString "ELet" . showChar ' ' . showsPrec 1 letdefs . showChar ' ' . showsPrec 1 exp . cpar n
    ECase exp cases -> opar n . showString "ECase" . showChar ' ' . showsPrec 1 exp . showChar ' ' . showsPrec 1 cases . cpar n
    EIf exp0 exp1 exp2 -> opar n . showString "EIf" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . showChar ' ' . showsPrec 1 exp2 . cpar n
    EDo binds exp -> opar n . showString "EDo" . showChar ' ' . showsPrec 1 binds . showChar ' ' . showsPrec 1 exp . cpar n
    EBind exp0 exp1 -> opar n . showString "EBind" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EBindC exp0 exp1 -> opar n . showString "EBindC" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EOr exp0 exp1 -> opar n . showString "EOr" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EAnd exp0 exp1 -> opar n . showString "EAnd" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EEq exp0 exp1 -> opar n . showString "EEq" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    ENe exp0 exp1 -> opar n . showString "ENe" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    ELt exp0 exp1 -> opar n . showString "ELt" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    ELe exp0 exp1 -> opar n . showString "ELe" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EGt exp0 exp1 -> opar n . showString "EGt" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EGe exp0 exp1 -> opar n . showString "EGe" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EListCons exp0 exp1 -> opar n . showString "EListCons" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EAdd exp0 exp1 -> opar n . showString "EAdd" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    ESub exp0 exp1 -> opar n . showString "ESub" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EMul exp0 exp1 -> opar n . showString "EMul" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EDiv exp0 exp1 -> opar n . showString "EDiv" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EMod exp0 exp1 -> opar n . showString "EMod" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    ENeg exp -> opar n . showString "ENeg" . showChar ' ' . showsPrec 1 exp . cpar n
    EApp exp0 exp1 -> opar n . showString "EApp" . showChar ' ' . showsPrec 1 exp0 . showChar ' ' . showsPrec 1 exp1 . cpar n
    EProj exp i -> opar n . showString "EProj" . showChar ' ' . showsPrec 1 exp . showChar ' ' . showsPrec 1 i . cpar n
    ERecType fieldtypes -> opar n . showString "ERecType" . showChar ' ' . showsPrec 1 fieldtypes . cpar n
    ERec fieldvalues -> opar n . showString "ERec" . showChar ' ' . showsPrec 1 fieldvalues . cpar n
    EEmptyList -> showString "EEmptyList"
    EList exps -> opar n . showString "EList" . showChar ' ' . showsPrec 1 exps . cpar n
    ETuple exp exps -> opar n . showString "ETuple" . showChar ' ' . showsPrec 1 exp . showChar ' ' . showsPrec 1 exps . cpar n
    EVar i -> opar n . showString "EVar" . showChar ' ' . showsPrec 1 i . cpar n
    EType -> showString "EType"
    EStr str -> opar n . showString "EStr" . showChar ' ' . showsPrec 1 str . cpar n
    EInteger n -> opar n . showString "EInteger" . showChar ' ' . showsPrec 1 n . cpar n
    EDouble d -> opar n . showString "EDouble" . showChar ' ' . showsPrec 1 d . cpar n
    EMeta -> showString "EMeta"
    VVar i -> opar n . showString "VVar" . showChar ' ' . showsPrec 1 i . cpar n
    VWild -> showString "VWild"
    LetDef i exp -> opar n . showString "LetDef" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 exp . cpar n
    Case pattern guard exp -> opar n . showString "Case" . showChar ' ' . showsPrec 1 pattern . showChar ' ' . showsPrec 1 guard . showChar ' ' . showsPrec 1 exp . cpar n
    BindVar varorwild exp -> opar n . showString "BindVar" . showChar ' ' . showsPrec 1 varorwild . showChar ' ' . showsPrec 1 exp . cpar n
    BindNoVar exp -> opar n . showString "BindNoVar" . showChar ' ' . showsPrec 1 exp . cpar n
    FieldType i exp -> opar n . showString "FieldType" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 exp . cpar n
    FieldValue i exp -> opar n . showString "FieldValue" . showChar ' ' . showsPrec 1 i . showChar ' ' . showsPrec 1 exp . cpar n
    Ident str -> opar n . showString "Ident" . showChar ' ' . showsPrec 1 str . cpar n
   where opar n = if n > 0 then showChar '(' else id
         cpar n = if n > 0 then showChar ')' else id

instance Eq (Tree c) where (==) = johnMajorEq

johnMajorEq :: Tree a -> Tree b -> Bool
johnMajorEq (Module imports decls) (Module imports_ decls_) = imports == imports_ && decls == decls_
johnMajorEq (Import i) (Import i_) = i == i_
johnMajorEq (DataDecl i exp consdecls) (DataDecl i_ exp_ consdecls_) = i == i_ && exp == exp_ && consdecls == consdecls_
johnMajorEq (TypeDecl i exp) (TypeDecl i_ exp_) = i == i_ && exp == exp_
johnMajorEq (ValueDecl i patterns guard exp) (ValueDecl i_ patterns_ guard_ exp_) = i == i_ && patterns == patterns_ && guard == guard_ && exp == exp_
johnMajorEq (DeriveDecl i0 i1) (DeriveDecl i0_ i1_) = i0 == i0_ && i1 == i1_
johnMajorEq (ConsDecl i exp) (ConsDecl i_ exp_) = i == i_ && exp == exp_
johnMajorEq (GuardExp exp) (GuardExp exp_) = exp == exp_
johnMajorEq GuardNo GuardNo = True
johnMajorEq (POr pattern0 pattern1) (POr pattern0_ pattern1_) = pattern0 == pattern0_ && pattern1 == pattern1_
johnMajorEq (PListCons pattern0 pattern1) (PListCons pattern0_ pattern1_) = pattern0 == pattern0_ && pattern1 == pattern1_
johnMajorEq (PConsTop i pattern patterns) (PConsTop i_ pattern_ patterns_) = i == i_ && pattern == pattern_ && patterns == patterns_
johnMajorEq (PCons i patterns) (PCons i_ patterns_) = i == i_ && patterns == patterns_
johnMajorEq (PRec fieldpatterns) (PRec fieldpatterns_) = fieldpatterns == fieldpatterns_
johnMajorEq PEmptyList PEmptyList = True
johnMajorEq (PList commapatterns) (PList commapatterns_) = commapatterns == commapatterns_
johnMajorEq (PTuple commapattern commapatterns) (PTuple commapattern_ commapatterns_) = commapattern == commapattern_ && commapatterns == commapatterns_
johnMajorEq PType PType = True
johnMajorEq (PStr str) (PStr str_) = str == str_
johnMajorEq (PInt n) (PInt n_) = n == n_
johnMajorEq (PVar i) (PVar i_) = i == i_
johnMajorEq PWild PWild = True
johnMajorEq (CommaPattern pattern) (CommaPattern pattern_) = pattern == pattern_
johnMajorEq (FieldPattern i pattern) (FieldPattern i_ pattern_) = i == i_ && pattern == pattern_
johnMajorEq (EPi varorwild exp0 exp1) (EPi varorwild_ exp0_ exp1_) = varorwild == varorwild_ && exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EPiNoVar exp0 exp1) (EPiNoVar exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EAbs varorwild exp) (EAbs varorwild_ exp_) = varorwild == varorwild_ && exp == exp_
johnMajorEq (ELet letdefs exp) (ELet letdefs_ exp_) = letdefs == letdefs_ && exp == exp_
johnMajorEq (ECase exp cases) (ECase exp_ cases_) = exp == exp_ && cases == cases_
johnMajorEq (EIf exp0 exp1 exp2) (EIf exp0_ exp1_ exp2_) = exp0 == exp0_ && exp1 == exp1_ && exp2 == exp2_
johnMajorEq (EDo binds exp) (EDo binds_ exp_) = binds == binds_ && exp == exp_
johnMajorEq (EBind exp0 exp1) (EBind exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EBindC exp0 exp1) (EBindC exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EOr exp0 exp1) (EOr exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EAnd exp0 exp1) (EAnd exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EEq exp0 exp1) (EEq exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (ENe exp0 exp1) (ENe exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (ELt exp0 exp1) (ELt exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (ELe exp0 exp1) (ELe exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EGt exp0 exp1) (EGt exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EGe exp0 exp1) (EGe exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EListCons exp0 exp1) (EListCons exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EAdd exp0 exp1) (EAdd exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (ESub exp0 exp1) (ESub exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EMul exp0 exp1) (EMul exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EDiv exp0 exp1) (EDiv exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EMod exp0 exp1) (EMod exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (ENeg exp) (ENeg exp_) = exp == exp_
johnMajorEq (EApp exp0 exp1) (EApp exp0_ exp1_) = exp0 == exp0_ && exp1 == exp1_
johnMajorEq (EProj exp i) (EProj exp_ i_) = exp == exp_ && i == i_
johnMajorEq (ERecType fieldtypes) (ERecType fieldtypes_) = fieldtypes == fieldtypes_
johnMajorEq (ERec fieldvalues) (ERec fieldvalues_) = fieldvalues == fieldvalues_
johnMajorEq EEmptyList EEmptyList = True
johnMajorEq (EList exps) (EList exps_) = exps == exps_
johnMajorEq (ETuple exp exps) (ETuple exp_ exps_) = exp == exp_ && exps == exps_
johnMajorEq (EVar i) (EVar i_) = i == i_
johnMajorEq EType EType = True
johnMajorEq (EStr str) (EStr str_) = str == str_
johnMajorEq (EInteger n) (EInteger n_) = n == n_
johnMajorEq (EDouble d) (EDouble d_) = d == d_
johnMajorEq EMeta EMeta = True
johnMajorEq (VVar i) (VVar i_) = i == i_
johnMajorEq VWild VWild = True
johnMajorEq (LetDef i exp) (LetDef i_ exp_) = i == i_ && exp == exp_
johnMajorEq (Case pattern guard exp) (Case pattern_ guard_ exp_) = pattern == pattern_ && guard == guard_ && exp == exp_
johnMajorEq (BindVar varorwild exp) (BindVar varorwild_ exp_) = varorwild == varorwild_ && exp == exp_
johnMajorEq (BindNoVar exp) (BindNoVar exp_) = exp == exp_
johnMajorEq (FieldType i exp) (FieldType i_ exp_) = i == i_ && exp == exp_
johnMajorEq (FieldValue i exp) (FieldValue i_ exp_) = i == i_ && exp == exp_
johnMajorEq (Ident str) (Ident str_) = str == str_
johnMajorEq _ _ = False

instance Ord (Tree c) where
  compare x y = compare (index x) (index y) `mappend` compareSame x y
    where
    index (Module _ _) = 0
    index (Import _) = 1
    index (DataDecl _ _ _) = 2
    index (TypeDecl _ _) = 3
    index (ValueDecl _ _ _ _) = 4
    index (DeriveDecl _ _) = 5
    index (ConsDecl _ _) = 6
    index (GuardExp _) = 7
    index (GuardNo ) = 8
    index (POr _ _) = 9
    index (PListCons _ _) = 10
    index (PConsTop _ _ _) = 11
    index (PCons _ _) = 12
    index (PRec _) = 13
    index (PEmptyList ) = 14
    index (PList _) = 15
    index (PTuple _ _) = 16
    index (PType ) = 17
    index (PStr _) = 18
    index (PInt _) = 19
    index (PVar _) = 20
    index (PWild ) = 21
    index (CommaPattern _) = 22
    index (FieldPattern _ _) = 23
    index (EPi _ _ _) = 24
    index (EPiNoVar _ _) = 25
    index (EAbs _ _) = 26
    index (ELet _ _) = 27
    index (ECase _ _) = 28
    index (EIf _ _ _) = 29
    index (EDo _ _) = 30
    index (EBind _ _) = 31
    index (EBindC _ _) = 32
    index (EOr _ _) = 33
    index (EAnd _ _) = 34
    index (EEq _ _) = 35
    index (ENe _ _) = 36
    index (ELt _ _) = 37
    index (ELe _ _) = 38
    index (EGt _ _) = 39
    index (EGe _ _) = 40
    index (EListCons _ _) = 41
    index (EAdd _ _) = 42
    index (ESub _ _) = 43
    index (EMul _ _) = 44
    index (EDiv _ _) = 45
    index (EMod _ _) = 46
    index (ENeg _) = 47
    index (EApp _ _) = 48
    index (EProj _ _) = 49
    index (ERecType _) = 50
    index (ERec _) = 51
    index (EEmptyList ) = 52
    index (EList _) = 53
    index (ETuple _ _) = 54
    index (EVar _) = 55
    index (EType ) = 56
    index (EStr _) = 57
    index (EInteger _) = 58
    index (EDouble _) = 59
    index (EMeta ) = 60
    index (VVar _) = 61
    index (VWild ) = 62
    index (LetDef _ _) = 63
    index (Case _ _ _) = 64
    index (BindVar _ _) = 65
    index (BindNoVar _) = 66
    index (FieldType _ _) = 67
    index (FieldValue _ _) = 68
    index (Ident _) = 69
    compareSame (Module imports decls) (Module imports_ decls_) = mappend (compare imports imports_) (compare decls decls_)
    compareSame (Import i) (Import i_) = compare i i_
    compareSame (DataDecl i exp consdecls) (DataDecl i_ exp_ consdecls_) = mappend (compare i i_) (mappend (compare exp exp_) (compare consdecls consdecls_))
    compareSame (TypeDecl i exp) (TypeDecl i_ exp_) = mappend (compare i i_) (compare exp exp_)
    compareSame (ValueDecl i patterns guard exp) (ValueDecl i_ patterns_ guard_ exp_) = mappend (compare i i_) (mappend (compare patterns patterns_) (mappend (compare guard guard_) (compare exp exp_)))
    compareSame (DeriveDecl i0 i1) (DeriveDecl i0_ i1_) = mappend (compare i0 i0_) (compare i1 i1_)
    compareSame (ConsDecl i exp) (ConsDecl i_ exp_) = mappend (compare i i_) (compare exp exp_)
    compareSame (GuardExp exp) (GuardExp exp_) = compare exp exp_
    compareSame GuardNo GuardNo = EQ
    compareSame (POr pattern0 pattern1) (POr pattern0_ pattern1_) = mappend (compare pattern0 pattern0_) (compare pattern1 pattern1_)
    compareSame (PListCons pattern0 pattern1) (PListCons pattern0_ pattern1_) = mappend (compare pattern0 pattern0_) (compare pattern1 pattern1_)
    compareSame (PConsTop i pattern patterns) (PConsTop i_ pattern_ patterns_) = mappend (compare i i_) (mappend (compare pattern pattern_) (compare patterns patterns_))
    compareSame (PCons i patterns) (PCons i_ patterns_) = mappend (compare i i_) (compare patterns patterns_)
    compareSame (PRec fieldpatterns) (PRec fieldpatterns_) = compare fieldpatterns fieldpatterns_
    compareSame PEmptyList PEmptyList = EQ
    compareSame (PList commapatterns) (PList commapatterns_) = compare commapatterns commapatterns_
    compareSame (PTuple commapattern commapatterns) (PTuple commapattern_ commapatterns_) = mappend (compare commapattern commapattern_) (compare commapatterns commapatterns_)
    compareSame PType PType = EQ
    compareSame (PStr str) (PStr str_) = compare str str_
    compareSame (PInt n) (PInt n_) = compare n n_
    compareSame (PVar i) (PVar i_) = compare i i_
    compareSame PWild PWild = EQ
    compareSame (CommaPattern pattern) (CommaPattern pattern_) = compare pattern pattern_
    compareSame (FieldPattern i pattern) (FieldPattern i_ pattern_) = mappend (compare i i_) (compare pattern pattern_)
    compareSame (EPi varorwild exp0 exp1) (EPi varorwild_ exp0_ exp1_) = mappend (compare varorwild varorwild_) (mappend (compare exp0 exp0_) (compare exp1 exp1_))
    compareSame (EPiNoVar exp0 exp1) (EPiNoVar exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EAbs varorwild exp) (EAbs varorwild_ exp_) = mappend (compare varorwild varorwild_) (compare exp exp_)
    compareSame (ELet letdefs exp) (ELet letdefs_ exp_) = mappend (compare letdefs letdefs_) (compare exp exp_)
    compareSame (ECase exp cases) (ECase exp_ cases_) = mappend (compare exp exp_) (compare cases cases_)
    compareSame (EIf exp0 exp1 exp2) (EIf exp0_ exp1_ exp2_) = mappend (compare exp0 exp0_) (mappend (compare exp1 exp1_) (compare exp2 exp2_))
    compareSame (EDo binds exp) (EDo binds_ exp_) = mappend (compare binds binds_) (compare exp exp_)
    compareSame (EBind exp0 exp1) (EBind exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EBindC exp0 exp1) (EBindC exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EOr exp0 exp1) (EOr exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EAnd exp0 exp1) (EAnd exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EEq exp0 exp1) (EEq exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (ENe exp0 exp1) (ENe exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (ELt exp0 exp1) (ELt exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (ELe exp0 exp1) (ELe exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EGt exp0 exp1) (EGt exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EGe exp0 exp1) (EGe exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EListCons exp0 exp1) (EListCons exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EAdd exp0 exp1) (EAdd exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (ESub exp0 exp1) (ESub exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EMul exp0 exp1) (EMul exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EDiv exp0 exp1) (EDiv exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EMod exp0 exp1) (EMod exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (ENeg exp) (ENeg exp_) = compare exp exp_
    compareSame (EApp exp0 exp1) (EApp exp0_ exp1_) = mappend (compare exp0 exp0_) (compare exp1 exp1_)
    compareSame (EProj exp i) (EProj exp_ i_) = mappend (compare exp exp_) (compare i i_)
    compareSame (ERecType fieldtypes) (ERecType fieldtypes_) = compare fieldtypes fieldtypes_
    compareSame (ERec fieldvalues) (ERec fieldvalues_) = compare fieldvalues fieldvalues_
    compareSame EEmptyList EEmptyList = EQ
    compareSame (EList exps) (EList exps_) = compare exps exps_
    compareSame (ETuple exp exps) (ETuple exp_ exps_) = mappend (compare exp exp_) (compare exps exps_)
    compareSame (EVar i) (EVar i_) = compare i i_
    compareSame EType EType = EQ
    compareSame (EStr str) (EStr str_) = compare str str_
    compareSame (EInteger n) (EInteger n_) = compare n n_
    compareSame (EDouble d) (EDouble d_) = compare d d_
    compareSame EMeta EMeta = EQ
    compareSame (VVar i) (VVar i_) = compare i i_
    compareSame VWild VWild = EQ
    compareSame (LetDef i exp) (LetDef i_ exp_) = mappend (compare i i_) (compare exp exp_)
    compareSame (Case pattern guard exp) (Case pattern_ guard_ exp_) = mappend (compare pattern pattern_) (mappend (compare guard guard_) (compare exp exp_))
    compareSame (BindVar varorwild exp) (BindVar varorwild_ exp_) = mappend (compare varorwild varorwild_) (compare exp exp_)
    compareSame (BindNoVar exp) (BindNoVar exp_) = compare exp exp_
    compareSame (FieldType i exp) (FieldType i_ exp_) = mappend (compare i i_) (compare exp exp_)
    compareSame (FieldValue i exp) (FieldValue i_ exp_) = mappend (compare i i_) (compare exp exp_)
    compareSame (Ident str) (Ident str_) = compare str str_
    compareSame x y = error "BNFC error:" compareSame
