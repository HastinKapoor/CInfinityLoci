import Mathlib.Analysis.Calculus.ContDiff.Defs
import Mathlib.Analysis.Calculus.ContDiff.Basic
import Mathlib.Analysis.Calculus.AffineMap
import Mathlib.Analysis.InnerProductSpace.PiL2
import Mathlib.Analysis.InnerProductSpace.Calculus
import Mathlib.Init.Function

-- namespace CinftyLoci

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
lemma dia_coe_comp {n m k: ℕ} (G : C^∞(ℝ^m, ℝ^k)) (F : C^∞(ℝ^n, ℝ^m)) : (G ⋄ F).1 = G.1 ∘ F.1 := rfl

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

-- Show that compositions of C^∞-Ring homomorphisms are C^∞-Ring homomorphisms?



/-
def sm_duple {n m k : ℕ} (F : C^∞(ℝ^n, ℝ^m)) (G : C^∞(ℝ^n, ℝ^k)) : C^∞(ℝ^n, ℝ^(m+k)) :=
  sorry

def sm_prod {n m j k : ℕ} (F : C^∞(ℝ^n, ℝ^j)) (G : C^∞(ℝ^m, ℝ^k)) : C^∞(ℝ^(n+m), ℝ^(j+k)) := by
  sorry
-/




def sm_tuple {n m : ℕ} (g : C^∞(ℝ^m)^n) : C^∞(ℝ^m, ℝ^n) := by
  use fun x => (fun j => g j x 0)
  apply contDiff_euclidean.2
  intro j
  exact contDiff_euclidean.1 (g j).2 0

@[simp]
lemma proj_tuple {n m : ℕ} (g : C^∞(ℝ^m)^n) (i : Fin n) : (π i) ⋄ (sm_tuple g) = g i := by
  ext _ i
  simp [sm_tuple, π]
  rw [Fin.fin_one_eq_zero i]

@[simp]
lemma intrprt_tuple {n m : ℕ} {A : Type _} [CinftyRing A] (g : C^∞(ℝ^m)^n) :
      intrprt (sm_tuple g) = fun (a : (Fin m → A)) i => (intrprt (g i) a) 0 := by
  ext a i
  let G := sm_tuple g
  calc intrprt (sm_tuple g) a i = intrprt (π i ⋄ G) a 0 := by simp [proj i, fnctr]
    _ = intrprt (g i) a 0 := by rw [proj_tuple g i]





-- Shows that C^∞(ℝ^d) is a C^∞-Ring
instance {d : ℕ} : CinftyRing C^∞(ℝ^d) where
  intrprt := by
    intro _ _ F g i
    exact (π i) ⋄ F ⋄ (sm_tuple g)
  fnctr := by intros; rfl
  proj := by
    intros
    ext _ _ _ j
    simp [π, sm_tuple]
    rw [Fin.fin_one_eq_zero j]

-- Shows that C^∞(R^d) is a free C^infty-Ring on the d generators π 1, ... , π d : C^∞(ℝ^d)
theorem free_CinftyRing (d : ℕ) : ∀ {A : Type _} [CinftyRing A] (a : A^d), ∃! Φ : CinftyRingHom C^∞(ℝ^d) A, (∀ i : Fin d, Φ (π i) = a i ) := by
  intro A _ a
  let Φ : CinftyRingHom C^∞(ℝ^d) A := by
    use fun f => intrprt f a 0
    intros; ext
    simp [intrprt, fnctr, proj]
    rfl
  use Φ
  constructor
  · simp [CinftyRing.proj]
  · intro Ψ h
    ext g
    let p : C^∞(ℝ^d)^d := (fun i => π i)
    calc
      Ψ g = (Ψ ∘ intrprt g p) 0 := by congr; ext; simp [intrprt]; rfl
      _ = intrprt g (Ψ ∘ p) 0 := by rw [Ψ.compat]
      _ = intrprt g a 0 := by congr; ext; exact h _




lemma A0_iso {A : Type _} : (Fin 0 → A) ≃ Fin 1 := {
  toFun := fun _ => 0
  invFun := fun _ => (nomatch ·)
  left_inv := by intro _; ext i; nomatch i
  right_inv := by intro _; exact (Fin.fin_one_eq_zero _).symm
}

lemma A1_iso {A : Type _} : (Fin 1 → A) ≃ A := {
  toFun := (· 0)
  invFun := fun a => (fun _ => a)
  left_inv := by intro _; ext i; simp; match i with | 0 => rfl
  right_inv := fun _ => rfl
}

lemma A2_iso {A : Type _} : (Fin 2 → A) ≃ A × A := {
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
  toFun := fun f => A1_iso (A1_iso (f ∘ A0_iso.invFun))
  invFun := fun b => A1_iso.invFun (A1_iso.invFun b) ∘ A0_iso
  left_inv := by intro _; simp [Function.comp]
  right_inv := by intro _; simp [Function.comp]
}

lemma A2toB1_iso {A B : Type _} : ((Fin 2 → A) → (Fin 1 → B)) ≃ (A → A → B) := {
  toFun := fun_of_cart_prod_iso ∘ (A1_iso.toFun ∘ · ∘ A2_iso.invFun)
  invFun := fun f => (A1_iso.invFun ∘ (fun_of_cart_prod_iso.invFun f) ∘ A2_iso.toFun)
  left_inv := by intro _; simp [Function.comp]
  right_inv := by intro _; simp [Function.comp]
}

def sm_add : C^∞(ℝ^2) := by
  use fun x _ => (x 0) + (x 1)
  apply contDiff_euclidean.2
  intro _
  exact ContDiff.add (contDiff_euclidean.1 contDiff_id 0) (contDiff_euclidean.1 contDiff_id 1)

def sm_mul : C^∞(ℝ^2) := by
  use fun x _ => (x 0) * (x 1)
  apply contDiff_euclidean.2
  intro _
  exact ContDiff.mul (contDiff_euclidean.1 contDiff_id 0) (contDiff_euclidean.1 contDiff_id 1)

def sm_twist : C^∞(ℝ^2, ℝ^2) := sm_tuple (match · with | 0 => π 1 | 1 => π 0)

def sm_neg : C^∞(ℝ^1) := ⟨fun x => -x, contDiff_neg⟩

def const_one (n : ℕ) : C^∞(ℝ^n) := ⟨fun _ _ => 1, contDiff_const⟩
def sm_one := const_one 0

def const_zero (n : ℕ) : C^∞(ℝ^n) := ⟨fun _ _ => 0, contDiff_const⟩
def sm_zero : C^∞(ℝ^0) := ⟨fun _ _ => 0, contDiff_const⟩

def sm_point (n : ℕ) : C^∞(ℝ^n, ℝ^0) := ⟨fun _ => (nomatch ·), contDiff_const⟩

@[simp]
lemma intrprt_twist {A : Type _} [inst : CinftyRing A] : (inst.intrprt sm_twist) = fun a => (match · with | 0 => a 1 | 1 => a 0) := by
  simp [sm_twist]
  ext _ i
  fin_cases i <;> simp [proj]




noncomputable instance {A : Type _} [CinftyRing A] : Zero A := {
  zero := A0toB1_iso (intrprt sm_zero)
}

noncomputable instance {A : Type _} [CinftyRing A] : One A := {
  one := A0toB1_iso (intrprt sm_one)
}

@[simp]
lemma intrprt_const_zero {A : Type _} [inst : CinftyRing A] {n : ℕ} : inst.intrprt (const_zero n) = fun _ _ => 0 := by
  ext a i
  have h : const_zero n = sm_zero ⋄ (sm_point n) := by ext; rfl
  calc
    intrprt (const_zero n) a i = (inst.intrprt sm_zero) (inst.intrprt (sm_point n) a) i := by simp [h, fnctr]
    _ = (inst.intrprt sm_zero) (A0_iso.invFun 0) i := by congr; ext j; nomatch j
    _ = A0toB1_iso (intrprt sm_zero) := by rw [Fin.fin_one_eq_zero i]; unfold A0toB1_iso; unfold A1_iso; rfl
    _ = 0 := rfl

@[simp]
lemma intrprt_const_one {A : Type _} [inst : CinftyRing A] {n : ℕ} : inst.intrprt (const_one n) = fun _ _ => 1 := by
  ext a i
  have h : const_one n = sm_one ⋄ (sm_point n) := by ext; rfl
  calc
    intrprt (const_one n) a i = (inst.intrprt sm_one) (inst.intrprt (sm_point n) a) i := by simp [h, fnctr]
    _ = (inst.intrprt sm_one) (A0_iso.invFun 0) i := by congr; ext j; nomatch j
    _ = A0toB1_iso (intrprt sm_one) := by rw [Fin.fin_one_eq_zero i]; unfold A0toB1_iso; unfold A1_iso; rfl
    _ = 1 := rfl



noncomputable instance {A : Type _} [CinftyRing A] : Add A := {
  add := A2toB1_iso (intrprt sm_add)
}

noncomputable instance {A : Type _} [CinftyRing A] : Mul A := {
  mul := A2toB1_iso (intrprt sm_mul)
}









lemma test {A : Type _} [CinftyRing A] : ∀ a : A, a + 0 = a := by
  intro a
  let f : C^∞(ℝ^1, ℝ^2) := ⟨fun x => (match · with | 0 => x 0 | 1 => 0), sorry⟩
  have h : sm_add = sm_add ⋄ sm_twist := by ext; simp [sm_add, sm_twist, sm_tuple, π, add_comm]
  calc
    a + 0 = a := sorry
  /-
  calc
    a + b = A2toB1_iso (intrprt sm_add) a b := rfl
    _ = A2toB1_iso (intrprt (sm_add ⋄ sm_twist)) a b := by nth_rw 1 [h]
    _ = A2toB1_iso (intrprt sm_add ∘ intrprt sm_twist) a b := by rw[fnctr]
    _ = (A1_iso.toFun ∘ (intrprt sm_add ∘ intrprt sm_twist) ∘ A2_iso.invFun) ⟨a, b⟩ := by unfold A2toB1_iso; unfold A1_iso; unfold fun_of_cart_prod_iso; simp
    _ = ((A1_iso.toFun ∘ intrprt sm_add ∘ A2_iso.invFun) ∘ (A2_iso.toFun ∘ intrprt sm_twist ∘ A2_iso.invFun)) ⟨a, b⟩ := by simp [A2_iso.left_inv]
    _ = (A1_iso.toFun ∘ intrprt sm_add ∘ A2_iso.invFun) ((A2_iso.toFun ∘ intrprt sm_twist ∘ A2_iso.invFun) ⟨a, b⟩) := rfl
    _ = (A1_iso.toFun ∘ intrprt sm_add ∘ A2_iso.invFun) ⟨b, a⟩ := by congr; unfold A2_iso; simp
    _ = A2toB1_iso (intrprt sm_add) b a := by unfold A2toB1_iso; unfold A1_iso; unfold fun_of_cart_prod_iso; simp
    _ = b + a := rfl
  -/








noncomputable instance {A : Type _} [CinftyRing A] : AddZeroClass A := {
  zero_add := sorry
  add_zero := sorry
}

noncomputable instance {A : Type _} [CinftyRing A] : Neg A := {
  neg := A1_iso.toFun ∘ (intrprt sm_neg) ∘ A1_iso.invFun
}

noncomputable instance {A : Type _} [CinftyRing A] : AddCommGroup A := {
  add_assoc := sorry
  zero_add := sorry
  add_zero := sorry
  nsmul := nsmulRec
  zsmul := zsmulRec
  add_left_neg := sorry
  add_comm := by
        intro a b
        have h : sm_add = sm_add ⋄ sm_twist := by ext; simp [sm_add, sm_twist, sm_tuple, π, add_comm]
        calc
          a + b = A2toB1_iso (intrprt sm_add) a b := rfl
          _ = A2toB1_iso (intrprt (sm_add ⋄ sm_twist)) a b := by nth_rw 1 [h]
          _ = A2toB1_iso (intrprt sm_add ∘ intrprt sm_twist) a b := by rw[fnctr]
          _ = (A1_iso.toFun ∘ (intrprt sm_add ∘ intrprt sm_twist) ∘ A2_iso.invFun) ⟨a, b⟩ := by unfold A2toB1_iso; unfold A1_iso; unfold fun_of_cart_prod_iso; simp
          _ = ((A1_iso.toFun ∘ intrprt sm_add ∘ A2_iso.invFun) ∘ (A2_iso.toFun ∘ intrprt sm_twist ∘ A2_iso.invFun)) ⟨a, b⟩ := by simp [A2_iso.left_inv]
          _ = (A1_iso.toFun ∘ intrprt sm_add ∘ A2_iso.invFun) ((A2_iso.toFun ∘ intrprt sm_twist ∘ A2_iso.invFun) ⟨a, b⟩) := rfl
          _ = (A1_iso.toFun ∘ intrprt sm_add ∘ A2_iso.invFun) ⟨b, a⟩ := by congr; unfold A2_iso; simp
          _ = A2toB1_iso (intrprt sm_add) b a := by unfold A2toB1_iso; unfold A1_iso; unfold fun_of_cart_prod_iso; simp
          _ = b + a := rfl
}




-- theorem saying that every C^∞-Ring is a commutative (unital) ring
noncomputable instance {A: Type _} [CinftyRing A] : CommRing A := {
  zsmul := zsmulRec
  mul_assoc := sorry
  mul_comm := by
      intro a b
      have h : sm_mul = sm_mul ⋄ sm_twist := by ext; simp [sm_mul, sm_twist, sm_tuple, π]; exact Real.commRing.proof_25 _ _
      calc
        a * b = A2toB1_iso (intrprt sm_mul) a b := rfl
        _ = A2toB1_iso (intrprt (sm_mul ⋄ sm_twist)) a b := by nth_rw 1 [h]
        _ = A2toB1_iso (intrprt sm_mul ∘ intrprt sm_twist) a b := by rw[fnctr]
        _ = (A1_iso.toFun ∘ (intrprt sm_mul ∘ intrprt sm_twist) ∘ A2_iso.invFun) ⟨a, b⟩ := by unfold A2toB1_iso; unfold A1_iso; unfold fun_of_cart_prod_iso; simp
        _ = ((A1_iso.toFun ∘ intrprt sm_mul ∘ A2_iso.invFun) ∘ (A2_iso.toFun ∘ intrprt sm_twist ∘ A2_iso.invFun)) ⟨a, b⟩ := by simp [A2_iso.left_inv]
        _ = (A1_iso.toFun ∘ intrprt sm_mul ∘ A2_iso.invFun) ((A2_iso.toFun ∘ intrprt sm_twist ∘ A2_iso.invFun) ⟨a, b⟩) := rfl
        _ = (A1_iso.toFun ∘ intrprt sm_mul ∘ A2_iso.invFun) ⟨b, a⟩ := by congr; unfold A2_iso; simp
        _ = A2toB1_iso (intrprt sm_mul) b a := by unfold A2toB1_iso; unfold A1_iso; unfold fun_of_cart_prod_iso; simp
        _ = b * a := rfl
  zero_mul := sorry
  mul_zero := sorry
  one_mul := sorry
  mul_one := sorry
  left_distrib := sorry
  right_distrib := sorry
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
