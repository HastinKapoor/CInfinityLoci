import Mathlib.Analysis.Calculus.ContDiff.Defs
import Mathlib.Analysis.InnerProductSpace.PiL2

-- namespace CinftyLoci

variable (n m : ℕ)

notation "ℝ^"n => EuclideanSpace ℝ (Fin n)
notation "C^∞(ℝ^"n", ℝ^"m")" => {f: (ℝ^n) → (ℝ^m) // ContDiff ℝ ⊤ f }
notation "C^∞(ℝ^"n")" => C^∞(ℝ^n, ℝ^1)


#check C^∞(ℝ^n, ℝ^m)

variable (f : C^∞(ℝ^n, ℝ^m))
#check f.1
#check f.2

-- How does one write an element with type EuclideanSpace ℝ (Fin n)?
-- How does one tell lean that the composition defines a map C^∞(ℝ^m, ℝ^k) × C^∞(ℝ^n, ℝ^m) → C^∞(ℝ^n, ℝ^k)?
-- How does one show that the coordinate projections ℝ^n → ℝ are elements of C^∞(ℝ^n, ℝ^1)?
-- How does one access the unique element of EuclideanSpace ℝ (Fin 0), or of the type (Fin 0) → α more generally?

-- Outline:

def π {n : ℕ} (i : Fin n) : C^∞(ℝ^n) := by
  let f : (ℝ^n) → (ℝ^1) := sorry
  have h : ContDiff ℝ ⊤ f := by sorry
  exact ⟨f, h⟩

-- Define structure of a C^∞-Ring α (a C^∞-Ring taking values in the type A)
class CinftyRing (A: Type*) where
  intrprt : ∀ {n m : ℕ} (f : C^∞(ℝ^n, ℝ^m)), (Fin n → A) → (Fin m → A)
--   fnctr : ∀ {n m k: ℕ} (f : C^∞(ℝ^n, ℝ^m)) (g : C^∞(ℝ^m, ℝ^k)), ... (something saying intrprt (f ∘ g) = (intrprt f) ∘ (intrprt g))
  proj : ∀ {n : ℕ} (i : Fin n), intrprt (π i) = fun (a : Fin n → A) ↦ (fun (_ : Fin 1) ↦ a i)

-- define a type/structure/attribute Hom A B of C^∞-Ring homomorphisms (A: C^∞-Ring α) to (B: C^∞-Ring β)
@[ext]
structure CinftyRingHom (A B : Type*) [CinftyRing A] [CinftyRing B] where
  toFun : A → B
  compat : ∀ {n m : ℕ} (f : C^∞(ℝ^n, ℝ^m)) (a : Fin n → A), (fun (i : Fin m) ↦ (toFun (CinftyRing.intrprt f a i))) = CinftyRing.intrprt f (fun (i : Fin n) ↦ toFun (a i))

-- theorem saying that every C^∞-Ring is an instance of a commutative ℝ-algebra
instance {A: Type*} [CinftyRing A] : --ℝ-algebra A :=
{
  sorry
}

-- theorem saying that C^∞-Ring homomorphism is a unital ℝ-algebra homomorphism

-- instance struct (n : ℕ) : C^∞-Ring C^∞(ℝ^n, ℝ^1)
instance (n : ℕ) : CinftyRing C^∞(ℝ^n) where
  intrprt := sorry
  -- fnctr := sorry
  proj := sorry

-- theorem free_C^∞-Ring (n: ℕ) : ∀ (A : C^∞-Ring α) (a: Fin n → A), ∃! Φ: Hom C^∞(ℝ^n) A, (∀ i: Fin n, Φ (π i) = a i )
-- where π i : C^∞(ℝ^n, ℝ^1) is the projection ℝ^n → ℝ onto the ith factor


-- def FinGen (A: C^∞-Ring): ∃ (n: ℕ) (Φ: Hom C^∞(ℝ^n) A), Surjective Φ

-- prove that if A is a C^∞-Ring and I is an ideal of A, then A/I has a C^∞-Ring structure such that the projection A → A/I is a C^∞-Ring homomorphism

-- theorem fin_gen_iff_quot_of_free (A: C^∞-Ring): FinGen A ↔ ∃ (n: Nat) (I: Ideal C^∞(ℝ^n, ℝ^1)), Isom A C^∞(ℝ^n, ℝ^1)/I

-- Steps for further on down the line (if time): define open/closed subobjects, meets and joins thereof, normality, etc.
