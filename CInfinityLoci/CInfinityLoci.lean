import Mathlib.Analysis.Calculus.ContDiff.Defs
import Mathlib.Analysis.Calculus.ContDiff.Basic
import Mathlib.Analysis.Calculus.AffineMap
import Mathlib.Analysis.InnerProductSpace.PiL2
import Mathlib.Analysis.InnerProductSpace.Calculus
import Mathlib.Init.Function

-- namespace CinftyLoci

variable (n m : ℕ)

notation A"^"n => Fin n → A
notation "ℝ^"n => EuclideanSpace ℝ (Fin n)
notation "C^∞(ℝ^"n", ℝ^"m")" => {f: (ℝ^n) → (ℝ^m) // ContDiff ℝ ⊤ f }
notation "C^∞(ℝ^"n")" => C^∞(ℝ^n, ℝ^1)

instance {n m : ℕ} : CoeFun C^∞(ℝ^n, ℝ^m) (fun _ ↦ (ℝ^n) → (ℝ^m)) where
  coe := fun f ↦ f.1

-- variable (f : C^∞(ℝ^n, ℝ^m))
-- #check f.1
-- #check f.2

-- How does one write an element with type EuclideanSpace ℝ (Fin n)?

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

lemma pi0_eq_id : π (i: Fin 1) = (id: (ℝ^1) → (ℝ^1)) := by
      ext x j
      dsimp [π]
      rw [Fin.fin_one_eq_zero i, Fin.fin_one_eq_zero j]

-- Defines composition as a map ⋄ : C^∞(ℝ^m, ℝ^k) × C^∞(ℝ^n, ℝ^m) → C^∞(ℝ^n, ℝ^k)
def comp {n m k: ℕ} (G : C^∞(ℝ^m, ℝ^k)) (F : C^∞(ℝ^n, ℝ^m)) : C^∞(ℝ^n, ℝ^k) := ⟨G.1 ∘ F.1, ContDiff.comp G.2 F.2⟩
infixr:75 " ⋄ " => comp

lemma dia_coe_comp {n m k: ℕ} (G : C^∞(ℝ^m, ℝ^k)) (F : C^∞(ℝ^n, ℝ^m)) : (G ⋄ F).1 = G.1 ∘ F.1 := by rfl

-- Defines the class C^∞-Rings
class CinftyRing (A: Type*) where
  intrprt : ∀ {n m : ℕ} (_ : C^∞(ℝ^n, ℝ^m)), (A^n) → (A^m)
  fnctr : ∀ {n m k: ℕ} (F : C^∞(ℝ^n, ℝ^m)) (G : C^∞(ℝ^m, ℝ^k)), intrprt (G ⋄ F) = (intrprt G) ∘ (intrprt F)
  proj : ∀ {n : ℕ} (i : Fin n), intrprt (π i) = fun a ↦ (fun _ ↦ a i)

open CinftyRing

-- Define the structure of C^∞-Ring homomorphisms A → B
@[ext]
structure CinftyRingHom (A B : Type*) [CinftyRing A] [CinftyRing B] where
  toFun : A → B
  compat : ∀ {n m : ℕ} (F : C^∞(ℝ^n, ℝ^m)) (a : A^n), toFun ∘ (intrprt F a) = intrprt F (toFun ∘ a)

instance [CinftyRing A] [CinftyRing B] : CoeFun (CinftyRingHom A B) (fun _ ↦ A → B) where
  coe := CinftyRingHom.toFun

attribute [coe] CinftyRingHom.toFun

-- define coercion to ℝ-algebra homomorphism?

-- Show that compositions of C^∞-Ring homomorphisms are C^∞-Ring homomorphisms

-- theorem saying that every C^∞-Ring is a commutative (unital) ring
instance {A: Type*} [CinftyRing A] : CommRing A where
  zero := by
    let c₀ : C^∞(ℝ^0) := ⟨fun _ _ ↦ 0, contDiff_const⟩
    exact intrprt c₀ Fin.elim0 0
  one := by
    let c₁ : C^∞(ℝ^0) := ⟨fun _ _ ↦ 1, contDiff_const⟩
    exact intrprt c₁ Fin.elim0 0
  add := by
    let sm_mul : C^∞(ℝ^2) := by
      use fun x ↦ (fun _ ↦ (x 0) * (x 1))
      apply contDiff_euclidean.2
      intro _
      sorry -- convert ContDiff.mul (π (0: Fin 2)).2 (π (1: Fin 2)).2

    intro a₀ a₁
    let a : Fin 2 → A := by
      intro i



    sorry
  add_comm := sorry
  add_assoc := sorry
  zero_add := sorry
  add_zero := sorry
  neg := by
    let sm_neg : C^∞(ℝ^1) := ⟨fun x ↦ -x, contDiff_neg⟩
    exact fun a ↦ intrprt sm_neg (fun (_: Fin 1) ↦ a) 0
  nsmul := sorry
  mul := sorry
  mul_assoc := sorry
  mul_comm := sorry
  zero_mul := sorry
  mul_zero := sorry
  one_mul := sorry
  mul_one := sorry
  left_distrib := sorry
  right_distrib := sorry
  zsmul := sorry
  add_left_neg := sorry


-- theorem saying that every C^∞-Ring is an ℝ-algebra
instance (A: Type*) [CinftyRing A] : Algebra ℝ A where
  smul := sorry
  toFun := sorry
  map_one' := sorry
  map_mul' := sorry
  map_zero' := sorry
  map_add' := sorry
  commutes' := sorry
  smul_def' := sorry

-- theorem saying that C^∞-Ring homomorphism is a unital ℝ-algebra homomorphism

-- Shows that C^∞(ℝ^d) is a C^∞-Ring
instance {d : ℕ} : CinftyRing C^∞(ℝ^d) where
  intrprt := by
    intro n _ F g i
    have G : C^∞(ℝ^d, ℝ^n) := by
      use fun x ↦ (fun j ↦ g j x 0)
      apply contDiff_euclidean.2
      intro j
      apply contDiff_euclidean.1
      exact (g j).2
    exact (π i) ⋄ F ⋄ G
  fnctr := by
    intro _ _ _ _ _
    rfl
  proj := by
    intro _ _
    ext _
    rw [dia_coe_comp, dia_coe_comp, pi0_eq_id, π]
    dsimp
    rename_i j
    rw [Fin.fin_one_eq_zero j]

theorem free_CinftyRing (d: ℕ) : ∀ {A: Type*} [CinftyRing A] (a: A^d), ∃! Φ : CinftyRingHom C^∞(ℝ^d) A, (∀ i : Fin d, Φ (π i) = a i ) := by
  intro A _ a
  let Φ : CinftyRingHom C^∞(ℝ^d) A := by
    use fun f ↦ intrprt f a 0
    intro n m F g
    ext i
    dsimp
    sorry -- apply CinftyRing.fnctr


  use Φ
  constructor
  · intro i
    dsimp
    rw [CinftyRing.proj i]
  · intro Ψ h
    ext g
    sorry

def fin_gen (A: Type*) [CinftyRing A] : Prop := ∃ (d : ℕ) (Φ: CinftyRingHom C^∞(ℝ^d) A), Function.Surjective Φ

-- prove that if A is a C^∞-Ring and I is an ideal of A, then A/I has a C^∞-Ring structure such that the projection A → A/I is a C^∞-Ring homomorphism

-- theorem fin_gen_iff_quot_of_free (A: C^∞-Ring): FinGen A ↔ ∃ (n: Nat) (I: Ideal C^∞(ℝ^n, ℝ^1)), Isom A C^∞(ℝ^n, ℝ^1)/I

-- Steps for further on down the line (if time): define open/closed subobjects, meets and joins thereof, normality, etc.
