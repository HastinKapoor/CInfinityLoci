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

instance {n m : ℕ} : CoeFun C^∞(ℝ^n, ℝ^m) (fun _ => (ℝ^n) → (ℝ^m)) where
  coe := fun f => f.1


-- How does one write an element with type EuclideanSpace ℝ (Fin n)?

-- defines the ith projection map π i : ℝ^n → ℝ
def π {n : ℕ} (i : Fin n) : C^∞(ℝ^n) := by
  use (fun x => (fun _ => x i))
  apply contDiff_euclidean.2
  exact fun _ => contDiff_euclidean.1 contDiff_id i

@[simp]
lemma pi0_eq_id : π (i: Fin 1) = (id: (ℝ^1) → (ℝ^1)) := by
      ext x j
      simp [π]
      rw [Fin.fin_one_eq_zero i, Fin.fin_one_eq_zero j]

-- Defines composition as a map ⋄ : C^∞(ℝ^m, ℝ^k) × C^∞(ℝ^n, ℝ^m) → C^∞(ℝ^n, ℝ^k)
def comp {n m k: ℕ} (G : C^∞(ℝ^m, ℝ^k)) (F : C^∞(ℝ^n, ℝ^m)) : C^∞(ℝ^n, ℝ^k) := ⟨G.1 ∘ F.1, ContDiff.comp G.2 F.2⟩
infixr:75 " ⋄ " => comp

@[simp]
lemma dia_coe_comp {n m k: ℕ} (G : C^∞(ℝ^m, ℝ^k)) (F : C^∞(ℝ^n, ℝ^m)) : (G ⋄ F).1 = G.1 ∘ F.1 := by rfl

-- Defines the class C^∞-Rings
class CinftyRing (A: Type _) where
  intrprt : ∀ {n m : ℕ} (_ : C^∞(ℝ^n, ℝ^m)), (A^n) → (A^m)
  fnctr : ∀ {n m k: ℕ} (F : C^∞(ℝ^n, ℝ^m)) (G : C^∞(ℝ^m, ℝ^k)), intrprt (G ⋄ F) = (intrprt G) ∘ (intrprt F)
  proj : ∀ {n : ℕ} (i : Fin n), intrprt (π i) = fun a _ => a i

open CinftyRing

-- Define the structure of C^∞-Ring homomorphisms A → B
@[ext]
structure CinftyRingHom (A B : Type _) [CinftyRing A] [CinftyRing B] where
  toFun : A → B
  compat : ∀ {n m : ℕ} (F : C^∞(ℝ^n, ℝ^m)) (a : A^n), toFun ∘ (intrprt F a) = intrprt F (toFun ∘ a)

instance [CinftyRing A] [CinftyRing B] : CoeFun (CinftyRingHom A B) (fun _ => A → B) where
  coe := CinftyRingHom.toFun

attribute [coe] CinftyRingHom.toFun

-- Show that compositions of C^∞-Ring homomorphisms are C^∞-Ring homomorphisms

-- Shows that C^∞(ℝ^d) is a C^∞-Ring
instance {d : ℕ} : CinftyRing C^∞(ℝ^d) where
  intrprt := by
    intro n _ F g i
    have G : C^∞(ℝ^d, ℝ^n) := by
      use fun x => (fun j => g j x 0)
      apply contDiff_euclidean.2
      intro j
      exact contDiff_euclidean.1 (g j).2 0
    exact (π i) ⋄ F ⋄ G
  fnctr := by
    intro _ _ _ _ _
    rfl
  proj := by
    intro _ _
    ext
    simp[π]
    rename_i j
    rw [Fin.fin_one_eq_zero j]

-- Shows that C^∞(R^d) is a free C^infty-Ring on the d generators π 1, ... , π d : C^∞(ℝ^d)
theorem free_CinftyRing (d : ℕ) : ∀ {A : Type _} [CinftyRing A] (a : A^d), ∃! Φ : CinftyRingHom C^∞(ℝ^d) A, (∀ i : Fin d, Φ (π i) = a i ) := by
  intro A _ a
  let Φ : CinftyRingHom C^∞(ℝ^d) A := by
    use fun f => intrprt f a 0
    intro n m _ _
    ext
    simp [intrprt, fnctr, proj]
    apply congrArg₂ _ _ rfl
    ext i
    have h : ∀ (b : A^n) (j : Fin n), b j = intrprt (π j) b 0 := by simp [proj]
    rw [h (intrprt _ a) i]
    apply congr_fun
    have t : ∀ (G: (A^d) → (A^n)) (H: (A^n) → (Fin 1 → A)) (b : (A^d)), H (G b) = (H ∘ G) b := by
      intro _ _ _
      rfl
    rw [t _ (intrprt (π i)) a, ← fnctr]
    apply congrArg₂ _ _ rfl
    ext _ j
    simp [π]
    rw [Fin.fin_one_eq_zero j]
  use Φ
  constructor
  · intro i
    dsimp
    rw [CinftyRing.proj i]
  · intro Ψ h
    ext g
    let p : C^∞(ℝ^d)^d := (fun i => π i)
    have t₁ : g = intrprt g p 0 := by
      ext
      simp [intrprt]
      rfl
    nth_rw 1 [t₁]
    calc
      Ψ (intrprt g p 0) = (Ψ ∘ intrprt g p) 0 := rfl
      _ = intrprt g (Ψ ∘ p) 0 := by rw [Ψ.compat]
      _ = intrprt g a 0 := by
        suffices t₂: Ψ ∘ p = a from by rw[t₂]
        ext
        exact h _




lemma fin0_iso {A : Type _} : (Fin 0 → A) ≃ Fin 1 := {
  toFun := fun _ => 0
  invFun := fun _ => (nomatch ·)
  left_inv := by intro _; ext i; nomatch i
  right_inv := by intro _; exact (Fin.fin_one_eq_zero _).symm
}

lemma fin1_iso {A : Type _} : (Fin 1 → A) ≃ A := {
  toFun := (· 0)
  invFun := fun a => (fun _ => a)
  left_inv := by intro _; ext i; simp; match i with | 0 => rfl
  right_inv := fun _ => rfl
}

lemma fin2_iso {A : Type _} : (Fin 2 → A) ≃ A × A := {
  toFun := fun a => ⟨a 0, a 1⟩
  invFun := fun ⟨a₀, a₁⟩ i => match i with | 0 => a₀ | 1 => a₁
  left_inv := by intro _; ext i; match i with | 0 => rfl | 1 => rfl
  right_inv := fun _ => rfl
}

lemma fun_of_cart_prod_iso {A B : Type _} : (A × A → B) ≃ (A → A → B) := {
  toFun := (· ⟨·, ·⟩)
  invFun := fun f a => f a.1 a.2
  left_inv := by intro _; ext; rfl
  right_inv := by intro _; ext; rfl
}

lemma A0toB1_iso {A B : Type _} : ((Fin 0 → A) → (Fin 1 → B)) ≃ B := {
  toFun := fun f => fin1_iso (fin1_iso (f ∘ fin0_iso.invFun))
  invFun := fun b => fin1_iso.invFun (fin1_iso.invFun b) ∘ fin0_iso
  left_inv := by intro _; simp [Function.comp]
  right_inv := by intro _; simp [Function.comp]
}

lemma A2toB1_iso {A B : Type _} : ((Fin 2 → A) → (Fin 1 → B)) ≃ (A → A → B) := {
  toFun := fun_of_cart_prod_iso ∘ (fin1_iso.toFun ∘ · ∘ fin2_iso.invFun)
  invFun := fun f => (fin1_iso.invFun ∘ (fun_of_cart_prod_iso.invFun f) ∘ fin2_iso.toFun)
  left_inv := by intro _; simp [Function.comp]
  right_inv := by intro _; simp [Function.comp]
}

def sm_add : C^∞(ℝ^2) := by
  use fun x => (fun _ => (x 0) + (x 1))
  apply contDiff_euclidean.2
  intro _
  exact ContDiff.add (contDiff_euclidean.1 contDiff_id 0) (contDiff_euclidean.1 contDiff_id 1)

def sm_mul : C^∞(ℝ^2) := by
  use fun x => (fun _ => (x 0) * (x 1))
  apply contDiff_euclidean.2
  intro _
  exact ContDiff.mul (contDiff_euclidean.1 contDiff_id 0) (contDiff_euclidean.1 contDiff_id 1)

def sm_twist : C^∞(ℝ^2, ℝ^2) := by
  use fun x => (match · with | 0 => x 1 | 1 => x 0)
  apply contDiff_euclidean.2
  intro i
  match i with
  | 0 => apply fun _ => contDiff_euclidean.1 contDiff_id 1
  | 1 => apply fun _ => contDiff_euclidean.1 contDiff_id 0

def sm_neg : C^∞(ℝ^1) := ⟨fun x => -x, contDiff_neg⟩

def sm_one : C^∞(ℝ^0) := ⟨fun _ _ => 1, contDiff_const⟩

def sm_zero : C^∞(ℝ^0) := ⟨fun _ _ => 0, contDiff_const⟩


-- theorem saying that every C^∞-Ring is a commutative (unital) ring
noncomputable
instance {A: Type u} [CinftyRing.{u} A] : CommRing.{u} A := {
  zero := A0toB1_iso (intrprt sm_zero)
  one := A0toB1_iso (intrprt sm_one)
  add := A2toB1_iso (intrprt sm_add)
  add_comm :=
    have h : sm_add = sm_add ⋄ sm_twist := by unfold comp; ext; simp [sm_twist, sm_add]; linarith
    have h2 : (intrprt (sm_add ⋄ sm_twist) = intrprt sm_add ∘ intrprt sm_twist) := fnctr sm_twist sm_add
    sorry
  add_assoc := sorry
  zero_add := sorry
  add_zero := sorry
  neg := fin1_iso.toFun ∘ (intrprt sm_neg) ∘ fin1_iso.invFun
  nsmul := sorry
  mul := A2toB1_iso (intrprt sm_mul)
  mul_assoc := sorry
  mul_comm :=
              have h : sm_mul = sm_mul ⋄ sm_twist := by unfold comp; ext; simp [sm_twist, sm_mul]; linarith
              /-
              have h2 : (intrprt (sm_mul ⋄ sm_twist) = intrprt sm_mul ∘ intrprt sm_twist) := fnctr sm_twist sm_mul
              -/
              sorry
  zero_mul := sorry
  mul_zero := sorry
  one_mul := sorry
  mul_one := sorry
  left_distrib := sorry
  right_distrib := sorry
  zsmul := zsmulRec
  add_left_neg := sorry
}

-- theorem saying that every C^∞-Ring is an ℝ-algebra
instance (A: Type _) [CinftyRing A] : Algebra ℝ A where
  smul := sorry
  toFun := sorry
  map_one' := sorry
  map_mul' := sorry
  map_zero' := sorry
  map_add' := sorry
  commutes' := sorry
  smul_def' := sorry

-- theorem saying that C^∞-Ring homomorphism is a unital ℝ-algebra homomorphism
-- define coercion to ℝ-algebra homomorphism?


def fin_gen (A: Type _) [CinftyRing A] : Prop := ∃ (d : ℕ) (Φ: CinftyRingHom C^∞(ℝ^d) A), Function.Surjective Φ

-- prove that if A is a C^∞-Ring and I is an ideal of A, then A/I has a C^∞-Ring structure such that the projection A → A/I is a C^∞-Ring homomorphism

-- theorem fin_gen_iff_quot_of_free (A: C^∞-Ring): FinGen A ↔ ∃ (n: Nat) (I: Ideal C^∞(ℝ^n, ℝ^1)), Isom A C^∞(ℝ^n, ℝ^1)/I

-- Steps for further on down the line (if time): define open/closed subobjects, meets and joins thereof, normality, etc.
