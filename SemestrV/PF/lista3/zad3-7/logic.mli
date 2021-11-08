(* UWAGA!
 *
 * Ten plik używa kodowania znaków UTF-8. Jeśli nie wyświetla się on
 * dobrze w Twoim edytorze, ustaw kodowanie znaków używane przez edytor
 * na UTF-8. Jeśli Twój edytor tego nie potrafi, to zmień edytor.
 *)

(** reprezentacja formuł *)
type formula = 
| Bottom
| Variable of string
| Implies of formula * formula

val pp_print_formula : Format.formatter -> formula -> unit

(** reprezentacja twierdzeń *)
(* type judgement
type theorem = private
| Ax       of judgement
| Impl_Int of judgement * theorem
| Impl_Elm of judgement * theorem * theorem
| Bott_Elm of judgement * theorem *)

type theorem

(** założenia twierdzenia *)
val assumptions : theorem -> formula list
(** teza twierdzenia *)
val consequence : theorem -> formula

val pp_print_theorem : Format.formatter -> theorem -> unit

(** by_assumption f konstruuje następujący dowód

  -------(Ax)
  {f} ⊢ f  *)
val by_assumption : formula -> theorem

(** imp_i f thm konstruuje następujący dowód

       thm
      Γ ⊢ φ
 ---------------(→I)
 Γ \ {f} ⊢ f → φ *)
val imp_i : formula -> theorem -> theorem

(** imp_e thm1 thm2 konstruuje następujący dowód

    thm1      thm2
 Γ ⊢ φ → ψ    Δ ⊢ φ 
 ------------------(→E)
 Γ ∪ Δ ⊢ ψ *)
val imp_e : theorem -> theorem -> theorem

(** bot_e f thm konstruuje następujący dowód

   thm
  Γ ⊢ ⊥
  -----(⊥E)
  Γ ⊢ f *)
val bot_e : formula -> theorem -> theorem
