# PCNF
Give one formula in FOL (first order logic) return a PCNF (prenex conjunctive normal form). </br>
Name Author: Camilo Andrés Rodríguez Garzón. </br>
Haskell versión: The Glorious Glasgow Haskell Compilation System, version 7.10.3 </br>
HLint versión: HLint v1.9.32, (C) Neil Mitchell 2006-2016 </br>

Syntactical Conventions:

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