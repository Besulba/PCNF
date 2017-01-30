# Pcnf
Camilo Andrés Rodríguez Garzón

Pcnf is a Haskell program intends to provide a solution
give one formula in FOL (first order logic) return a 
PCNF (prenex conjunctive normal form).
Haskell versión: The Glorious Glasgow Haskell Compilation System, version 7.10.3
HLint versión: HLint v1.9.35, (C) Neil Mitchell 2006-2016

### Syntactic Proposition

The set of well-formed formulae is defined by the following grammar:

    Formula =
    Pred Ft [Term]          (predicado)
    Not Formula             (negation)
    And Formula Formula     (conjunction)
    Or Formula Formula      (disjunction)
    Imp Formula Formula     (implication)
    Biimp Formula Formula   (bi-implication)
    Forall Term Formula     (universal quantifier)
    Exists Term Formula     (existential quantifier)