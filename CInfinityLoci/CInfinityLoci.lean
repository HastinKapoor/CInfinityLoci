-- Outline:

-- Define structure of a C^∞-Ring α (a C^∞-Ring taking values in the type α)

-- define a type Hom A B of C^∞-Ring homomorphisms (A: C^∞-Ring α) to (B: C^∞-Ring β)

-- instance struct (n : ℕ) : C^∞-Ring C^∞(𝓡 n, ℝ)

-- theorem free_C^∞-Ring (n: ℕ) : ∀ (A : C^∞-Ring α) (a: Fin n → A), ∃! Φ: Hom C^∞(𝓡 n, ℝ) A, (∀ i: Fin n, Φ (π i) = a i )
-- where π i : C^∞(𝓡 n, ℝ) is the projection ℝ^n → ℝ onto the ith factor


-- def FinGen (A: C^∞-Ring): ∃ (n: ℕ) (Φ: Hom C^∞(𝓡 n, ℝ) A), Surjective Φ

-- prove that if A is a C^∞-Ring and I is an ideal of A, then A/I has a C^∞-Ring structure such that the projection A → A/I is a C^∞-Ring homomorphism

-- theorem fin_gen_iff_quot_of_free (A: C^∞-Ring): FinGen A ↔ ∃ (n: Nat) (I: Ideal C^∞(𝓡 n, ℝ)), Isom A C^∞(ℝ n, ℝ)/I

-- Steps for further on down the line (if time): define open/closed subobjects, meets and joins thereof, normality, etc.
