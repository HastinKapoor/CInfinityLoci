import Mathlib.Analysis.Calculus.ContDiff.Defs
import Mathlib.Analysis.Calculus.ContDiff.Basic
import Mathlib.Analysis.Calculus.AffineMap
import Mathlib.Analysis.InnerProductSpace.PiL2
import Mathlib.Analysis.InnerProductSpace.Calculus

-- namespace CinftyLoci

variable (n m : ℕ)

notation A"^"n => Fin n → A
notation "ℝ^"n => EuclideanSpace ℝ (Fin n)
notation "C^∞(ℝ^"n", ℝ^"m")" => {f: (ℝ^n) → (ℝ^m) // ContDiff ℝ ⊤ f }
notation "C^∞(ℝ^"n")" => C^∞(ℝ^n, ℝ^1)

-- variable (f : C^∞(ℝ^n, ℝ^m))
-- #check f.1
-- #check f.2

-- How does one write an element with type EuclideanSpace ℝ (Fin n)?
-- How does one access the unique element of EuclideanSpace ℝ (Fin 0), or of the type (Fin 0) → α more generally?


-- defines the ith projection map π i : ℝ^n → ℝ
def π {n : ℕ} (i : Fin n) : C^∞(ℝ^n) := by
  use (fun x ↦ (fun _ ↦ x i))
  have h : ContDiff ℝ ⊤ (id : (ℝ^n) → (ℝ^n)) := contDiff_id
  convert contDiff_euclidean.1 h i
  constructor
  · intro t
    exact contDiff_euclidean.1 t 1
  · intro t
    apply contDiff_euclidean.2
    exact fun _ ↦ t

-- Defines composition as a map ⋄ : C^∞(ℝ^m, ℝ^k) × C^∞(ℝ^n, ℝ^m) → C^∞(ℝ^n, ℝ^k)
def comp {n m k: ℕ} (G : C^∞(ℝ^m, ℝ^k)) (F : C^∞(ℝ^n, ℝ^m)) : C^∞(ℝ^n, ℝ^k) := ⟨G.1 ∘ F.1, ContDiff.comp G.2 F.2⟩
infixr:75 " ⋄ " => comp

-- Defines the class C^∞-Rings
class CinftyRing (A: Type*) where
  intrprt : ∀ {n m : ℕ} (_ : C^∞(ℝ^n, ℝ^m)), (A^n) → (A^m)
  fnctr : ∀ {n m k: ℕ} (F : C^∞(ℝ^n, ℝ^m)) (G : C^∞(ℝ^m, ℝ^k)), intrprt (G ⋄ F) = (intrprt G) ∘ (intrprt F)
  proj : ∀ {n : ℕ} (i : Fin n), intrprt (π i) = fun a ↦ (fun _ ↦ a i)

-- Define the structure of C^∞-Ring homomorphisms A → B
@[ext]
structure CinftyRingHom (A B : Type*) [CinftyRing A] [CinftyRing B] where
  toFun : A → B
  compat : ∀ {n m : ℕ} (F : C^∞(ℝ^n, ℝ^m)) (a : A^n), toFun ∘ (CinftyRing.intrprt F a) = CinftyRing.intrprt F (toFun ∘ a)

instance [CinftyRing A] [CinftyRing B] : CoeFun (CinftyRingHom A B) (fun _ ↦ A → B) where
  coe := CinftyRingHom.toFun

attribute [coe] CinftyRingHom.toFun

-- define coercion to ℝ-algebra homomorphism?

-- Show that compositions of C^∞-Ring homomorphisms are C^∞-Ring homomorphisms

-- theorem saying that every C^∞-Ring is a commutative ℝ-algebra
instance {A: Type*} [CinftyRing A] : --ℝ-algebra A :=
{
  sorry
}

-- theorem saying that C^∞-Ring homomorphism is a unital ℝ-algebra homomorphism

-- Shows that C^∞(ℝ^d) is a C^∞-Ring
instance (d : ℕ) : CinftyRing C^∞(ℝ^d) where
  intrprt := by
    intro n m F g i
    let Fig: (ℝ^d) → (ℝ^1) := fun x ↦ ((π i) ⋄ F).1 (fun j ↦ (g j).1 x 0)
    have h : ContDiff ℝ ⊤ Fig := sorry
    exact ⟨Fig, h⟩
  fnctr := by
    intro _ _ _ _ _
    rfl
  proj := by
    intro n i
    ext g
    dsimp
    have h : π (0: Fin 1) = (id: (ℝ^1) → (ℝ^1)) := by
      ext x i
      dsimp
      sorry

    sorry

-- theorem free_C^∞-Ring (n: ℕ) : ∀ (A : C^∞-Ring α) (a: Fin n → A), ∃! Φ: Hom C^∞(ℝ^n) A, (∀ i: Fin n, Φ (π i) = a i )


-- def FinGen (A: C^∞-Ring): ∃ (n: ℕ) (Φ: Hom C^∞(ℝ^n) A), Surjective Φ

-- prove that if A is a C^∞-Ring and I is an ideal of A, then A/I has a C^∞-Ring structure such that the projection A → A/I is a C^∞-Ring homomorphism

-- theorem fin_gen_iff_quot_of_free (A: C^∞-Ring): FinGen A ↔ ∃ (n: Nat) (I: Ideal C^∞(ℝ^n, ℝ^1)), Isom A C^∞(ℝ^n, ℝ^1)/I

-- Steps for further on down the line (if time): define open/closed subobjects, meets and joins thereof, normality, etc.
