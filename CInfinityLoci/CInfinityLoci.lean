import Mathlib.Analysis.Calculus.ContDiff.Defs
import Mathlib.Analysis.InnerProductSpace.PiL2

namespace CInfinityLoci

variable (n m : ℕ)

notation "ℝ^"n => EuclideanSpace ℝ (Fin n)

notation "C^∞(ℝ^"n", ℝ^"m")" => {f: EuclideanSpace ℝ (Fin n) → EuclideanSpace ℝ (Fin m) // ContDiff ℝ (⊤ : ℕ∞) f }

#check C^∞(ℝ^n, ℝ^m)

-- How does one write an element with type EuclideanSpace ℝ (Fin n)?
-- How does one tell lean that the composition defines a map C^∞(ℝ^m, ℝ^k) × C^∞(ℝ^n, ℝ^m) → C^∞(ℝ^n, ℝ^k)?
-- How does one show that the coordinate projections ℝ^n → ℝ are elements of C^∞(ℝ^n, ℝ^1)?
-- How does one access the unique element of EuclideanSpace ℝ (Fin 0), or of the type (Fin 0) → α more generally?

-- Outline:

-- Define structure of a C^∞-Ring α (a C^∞-Ring taking values in the type α)
structure CinftyRing (α: Type*) where
  intrprt : ∀ {n m: ℕ} (f : C^∞(ℝ^n, ℝ^m)), (Fin n → α) → (Fin m → α)
--   fnctr : ∀ {n m k: ℕ} (f : C^∞(ℝ^n, ℝ^m)) (g : C^∞(ℝ^m, ℝ^k)), ... (something saying intrprt (f ∘ g) = (intrprt f) ∘ (intrprt g))
--   proj : ... (something saying that intrprt takes projections ℝ^n → ℝ onto the ith factor to projections (Fin n → α) → α onto the ith factor)


-- define a type Hom A B of C^∞-Ring homomorphisms (A: C^∞-Ring α) to (B: C^∞-Ring β)

-- theorem saying that every C^∞-Ring is an instance of an ℝ-algebra

-- theorem saying that C^∞-Ring homomorphism is an ℝ-algebra homomorphism

-- instance struct (n : ℕ) : C^∞-Ring C^∞(𝓡 n, ℝ)

-- theorem free_C^∞-Ring (n: ℕ) : ∀ (A : C^∞-Ring α) (a: Fin n → A), ∃! Φ: Hom C^∞(𝓡 n, ℝ) A, (∀ i: Fin n, Φ (π i) = a i )
-- where π i : C^∞(𝓡 n, ℝ) is the projection ℝ^n → ℝ onto the ith factor


-- def FinGen (A: C^∞-Ring): ∃ (n: ℕ) (Φ: Hom C^∞(𝓡 n, ℝ) A), Surjective Φ

-- prove that if A is a C^∞-Ring and I is an ideal of A, then A/I has a C^∞-Ring structure such that the projection A → A/I is a C^∞-Ring homomorphism

-- theorem fin_gen_iff_quot_of_free (A: C^∞-Ring): FinGen A ↔ ∃ (n: Nat) (I: Ideal C^∞(𝓡 n, ℝ)), Isom A C^∞(ℝ n, ℝ)/I

-- Steps for further on down the line (if time): define open/closed subobjects, meets and joins thereof, normality, etc.
