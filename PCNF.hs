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
--  (Forall x β ∨ σ)    ≈ Forall x (β ∨ σ) if x not pertain to Var Free of σ.
--  (Exists x α ∧ σ)    ≈ Exists x (α ∧ σ) if x not pertain to Var Free of σ.
extract :: Formula -> Formula
extract (Not f)       = Not $ extract f
extract (And (Forall x f1) f2)  = extract(Forall x (And f1 f2))    
extract (And f1 (Forall x f2))  = extract(Forall x (And f1 f2))
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
extract (Imp f1 f2)         = extract(Or (Not nf1) nf2)
  where
    nf1 = recursiveExtract f1
    nf2 = recursiveExtract f2
extract (Biimp f1 f2) = extract(And (Imp nf1 nf2) (Imp nf2 nf1))
  where
    nf1 = recursiveExtract f1
    nf2 = recursiveExtract f2
extract (Forall x f)   = if getEqTerm x nf then Forall x (extract f) else extract f
   where
      nf = getTerms f
extract (Exists x f)   = if getEqTerm x nf then Exists x (extract f) else extract f
   where
      nf = getTerms f
extract formula             = formula

-- call recursively to extract the (extract) code intern
recursiveExtract  :: Formula -> Formula
recursiveExtract = extract

-- getTerms: Function to get a array of Terms of a Formula
getTerms :: Formula -> [Term]
getTerms (Pred f1 f2) = f2
getTerms (Not f1) = nt1
    where 
    nt1 = getTerms f1
getTerms (And f1 f2) = merge nt1 nt2
  where 
    nt1 = getTerms f1
    nt2 = getTerms f2
getTerms (Or f1 f2) =  merge nt1 nt2
  where 
    nt1 = getTerms f1
    nt2 = getTerms f2
getTerms (Imp f1 f2) = merge nt1 nt2
  where 
    nt1 = getTerms f1
    nt2 = getTerms f2 
getTerms (Biimp f1 f2) = merge nt1 nt2
  where 
    nt1 = getTerms f1
    nt2 = getTerms f2  
getTerms (Forall x f1) = nt1
  where 
    nt1 = getTerms f1
getTerms (Exists x f1) = nt1
  where 
    nt1 = getTerms f1

-- getEqTerm: Function to compare a array with one Term, return true if exists el terms in the array Otherwise returns false
getEqTerm :: Term -> [Term] -> Bool
getEqTerm fv = foldr (\ t -> (||) (fv == t)) False

-- merge: concatenate two array of a same element
merge :: [a] -> [a] -> [a]
merge xs     []     = xs
merge []     ys     = ys
merge (x:xs) (y:ys) = x : y : merge xs ys