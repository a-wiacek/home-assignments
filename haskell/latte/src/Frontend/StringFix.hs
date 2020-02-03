module Frontend.StringFix(fixStrings) where

import AbsLatte

-- Using BNFC with --functor options yields position of each parsed symbol.
-- This is VERY useful for generating better error messages, but comes with a cost:
-- there is a bug such that String quotes are not discarded and expression "xyz\"\""
-- is parsed as "\"xyz\\\"\\\"\"" (Idents are parsed properly). This is band-aid fix.

class Fix a where fix :: a -> a

instance Fix (Program a) where
    fix (Program a defs) = Program a (map fix defs)

instance Fix (TopDef a) where
    fix (TopFunDef a def) = TopFunDef a (fix def)
    fix (BaseClassDef a ident members) = BaseClassDef a ident (map fix members)
    fix (ExtClassDef a ident1 ident2 members) = ExtClassDef a ident1 ident2 (map fix members)

instance Fix (FunDef a) where
    fix (FunDef a t ident args block) = FunDef a t ident args (fix block)

instance Fix (ClassMember a) where
    fix (ClassMethodDef a def) = ClassMethodDef a (fix def)
    fix m = m

instance Fix (Block a) where
    fix (StmtBlock a stmts) = StmtBlock a (map fix stmts)

instance Fix (Stmt a) where
    fix (SEmpty a) = SEmpty a
    fix (SBlock a block) = SBlock a (fix block)
    fix (SDecl a decl) = SDecl a (fix decl)
    fix (SAssign a lval exp) = SAssign a (fix lval) (fix exp)
    fix (SIncr a lval) = SIncr a (fix lval)
    fix (SDecr a lval) = SDecr a (fix lval)
    fix (SRet a exp) = SRet a (fix exp)
    fix (SRetVoid a) = SRetVoid a
    fix (SIf a exp stmt) = SIf a (fix exp) (fix stmt)
    fix (SIfte a exp stmt1 stmt2) = SIfte a (fix exp) (fix stmt1) (fix stmt2)
    fix (SWhile a exp stmt) = SWhile a (fix exp) (fix stmt)
    fix (SFor a t ident exp stmt) = SFor a t ident (fix exp) (fix stmt)
    fix (SExp a exp) = SExp a (fix exp)

instance Fix (Decl a) where
    fix (VarDecl a t items) = VarDecl a t (map fix items)

instance Fix (DeclItem a) where
    fix (DeclWithInit a ident exp) = DeclWithInit a ident (fix exp)
    fix i = i

instance Fix (LVal a) where
    fix (LValAttr a lval ident) = LValAttr a (fix lval) ident
    fix (LValCall a call) = LValCall a (fix call)
    fix (LValMethod a lval call) = LValMethod a (fix lval) (fix call)
    fix (LValArrAccess a lval exp) = LValArrAccess a (fix lval) (fix exp)
    fix lval = lval

instance Fix (Call a) where
    fix (FunCall a ident exps) = FunCall a ident (map fix exps)

instance Fix (Exp a) where
    fix (ELVal a lval) = ELVal a (fix lval)
    fix (ENewArr a t exp) = ENewArr a t (fix exp)
    fix (EConstant a c) = EConstant a (fix c)
    fix (ENegate a op exp) = ENegate a op (fix exp)
    fix (EMod a exp1 op exp2) = EMod a (fix exp1) op (fix exp2)
    fix (EAdd a exp1 op exp2) = EAdd a (fix exp1) op (fix exp2)
    fix (EComp a exp1 op exp2) = EComp a (fix exp1) op (fix exp2)
    fix (EAnd a exp1 exp2) = EAnd a (fix exp1) (fix exp2)
    fix (EOr a exp1 exp2) = EOr a (fix exp1) (fix exp2)
    fix exp = exp

instance Fix (Constant a) where
    fix (CString a s) = CString a (read s)
    fix c = c

fixStrings :: Program Pos -> Program Pos
fixStrings = fix