module PCNF where

import FOL
import CNF

-- Function to get the PCNF from a given formula in FOL.

pcnf :: Formula -> Formula
pcnf = demorgan . remImp . remBiimp . extract . dist . renameFormula

-- TODO renameFormula: Function to rename the bound variables, such that
-- there is not a variables binded by two differente quantifiers.
renameFormula :: Formula -> Formula
renameFormula f = f

-- extract: Function to extract the quantifiers from the matrix to
-- prefix.
--  (Forall x β ∨ σ)    ≈ Forall x (β ∨ σ).
--  (Exists α ∧ σ)      ≈ Exists x (α ∧ σ).
extract :: Formula -> Formula
--extract (And (Forall x1 (Pred f1 f2)) (Forall x2 (Pred f3 f4))) = if getEqTerm x1 f2  && getEqTerm x2 f4 && x1==x2 then Forall x1 (And (Pred f1 f2) (Pred f3 f4)) else And (Forall x1 (Pred f1 f2)) (Forall x2 (Pred f3 f4))
--extract (And (Forall x f1) f2)  = extract(Forall x (And f1 f2))
extract (And (Forall x f1) f2)  = if getFormulaSimple f1 && getFormulaSimple f2 && getEqTerm x nt1 && getEqTerm x nt2 == False then Forall x (And f1 f2) else And (extract (Forall x f1)) (extract f2)
  where
    nt1 = getTerms f1
    nt2 = getTerms f2

extract (And f1 (Forall x f2))  = if getFormulaSimple f1 && getFormulaSimple f2 && getEqTerm x nt1 == False && getEqTerm x nt2 then Forall x (And f1 f2) else And (extract f1) (extract (Forall x f2))
  where
    nt1 = getTerms f1
    nt2 = getTerms f2
    
--extract (And f1 (Forall x f2))  = extract(Forall x (And f1 f2))
extract (And (Exists x f1) f2)  = extract(Exists x (And f1 f2))
extract (And f1 (Exists x f2))  = extract(Exists x (And f1 f2))
extract (And f1 f2)   = if (nf1 == f1) && (nf2 == f2) then And nf1 nf2 else extract (And nf1 nf2)
  where
    nf1 = recursiveExtract f1
    nf2 = recursiveExtract f2
extract (Or (Exists x1 (Pred f1 f2)) (Exists x2 (Pred f3 f4))) = if getEqTerm x1 f2  && getEqTerm x2 f4 && x1==x2 then Exists x1 (Or (Pred f1 f2) (Pred f3 f4)) else Or (Exists x1 (Pred f1 f2)) (Exists x2 (Pred f3 f4))
extract (Or (Forall x f1) f2)   = extract(Forall x (Or f1 f2))
extract (Or f1 (Forall x f2))   = extract(Forall x (Or f1 f2))
extract (Or (Exists x f1) f2)   = extract(Exists x (Or f1 f2))
extract (Or f1 (Exists x f2))   = extract(Exists x (Or f1 f2))
extract (Or f1 f2)   = if (nf1 == f1) && (nf2 == f2) then Or nf1 nf2 else extract (Or nf1 nf2)
  where
    nf1 = recursiveExtract f1
    nf2 = recursiveExtract f2
extract (Imp f1 f2)  = Or (Not $ extract f1) (extract f2)
extract (Biimp f1 f2) = And (Imp nf1 nf2) (Imp nf2 nf1)
  where
    nf1 = recursiveExtract f1
    nf2 = recursiveExtract f2
extract (Forall x f)   = if getFormulaSimple f then if getEqTerm x nf then Forall x f else f else Forall x (extract f)
   where
      nf = getTerms f
extract (Exists x f)   = if getFormulaSimple f then if getEqTerm x nf then Exists x f else f else Exists x (extract f)
   where
      nf = getTerms f
extract formula             = formula
---------------------------------------------------------
-- call recursively to extract the (extract) code intern
recursiveExtract  :: Formula -> Formula
recursiveExtract = extract

-- getTerms: Function to get a array of Terms
getTerms :: Formula -> [Term]
--getTerms (And (Pred f1 f2) (Pred f3 f4)) = f2 + f4
getTerms (Pred f1 f2) = f2
getTerms f = []

-- getFormulaSimple: Function that return True if the Formula is simple (Pred f f1) false in otherwise
getFormulaSimple :: Formula -> Bool
getFormulaSimple (Pred f f1) = True
getFormulaSimple f = False

-- getEqTerm: Function to compare a array with one Term, return true if exists el terms in the array Otherwise returns false
getEqTerm :: Term -> [Term] -> Bool
getEqTerm fv = foldr (\ t -> (||) (fv == t)) False

