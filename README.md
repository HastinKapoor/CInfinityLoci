### Goal:
This github repo aims to formalize in Lean some basic aspects of the theory of C^infty rings, such
as basic definitions, free C^infty rings, proving that C^infty rings are in fact rings, quotients
of C^infty rings, pointed C^infty rings, etc. Time permitting, we will also attempt to define
localizations of C^infty rings and tensor products of C^infty rings (i.e., the coproduct in the
category of C^infty rings, not in the category rings), define C^infty loci and open/closed
subobjects, and establish some basic geometrical properties.

### References:
"Models for Smooth Infinitesimal Analysis" by Moerdijk and Reyes (in particular the first chapter)

### Definitions:
The function spaces C^∞(ℝ^n, ℝ^m) for n, m in ℕ are defined as the subtype
```
  {f: (ℝ^n) → (ℝ^m) // ContDiff ℝ ⊤ f }
```
with several appropriate coercions and extensionality theorems.

C^infty rings are defined by
```
class CinftyRing (A: Type _) where
  intrprt : ∀ {n m : ℕ} (_ : C^∞(ℝ^n, ℝ^m)), (A^n) → (A^m)
  fnctr : ∀ {n m k: ℕ} (F : C^∞(ℝ^n, ℝ^m)) (G : C^∞(ℝ^m, ℝ^k)), intrprt (G ⋄ F) = (intrprt G) ∘ (intrprt F)
  proj : ∀ {n : ℕ} (i : Fin n), intrprt (π i) = fun a _ => a i
```

C^infty ring homomorphisms are defined as the structure
```
structure CinftyRingHom (A B : Type _) [CinftyRing A] [CinftyRing B] where
  toFun : A → B
  compat : ∀ {n m : ℕ} (F : C^∞(ℝ^n, ℝ^m)) (a : A^n), toFun ∘ (intrprt F a) = intrprt F (toFun ∘ a)
```

### Main Theorems:
We showed that C^∞(ℝ^d) is a C^infty ring, and further showed that it is in fact the free C^infty ring
on d generators. The next main theorem (work in progress) is showing that any C^infty Ring is a commutative
unital ℝ-algebra.

### Future Work:
Complete proof that C^infty rings are ℝ-algebras, prove the geometrical characterizations of C^infty
ring homomorphisms between finitely generated ℝ-algebras (and other geometrical properties of ℝ-algebras),
define C^infty loci and establish their geometrical properties.
