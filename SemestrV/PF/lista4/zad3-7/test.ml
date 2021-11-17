open Proof
open Logic

(* #install_printer Logic.pp_print_formula;;
#install_printer Logic.pp_print_theorem;;
#install_printer Proof.pp_print_proof;; *)

let f1 = (
  Implies(
    Variable("p"), 
    Implies( 
      Implies( 
        Variable("p"), 
        Variable("q")
      ), 
      Variable("q")
    )
  )
)

let t1 = 
  proof [] f1
    |> intro "H1"
    |> intro "H2"
    |> apply_assm "H2"
    |> apply_assm "H1"
  |> qed


let f2 = (
  Implies(
    Implies(
      Variable("p"),
      Implies(
        Variable("q"),
        Variable("r")
      )
    ),
    Implies(
      Implies(
        Variable("p"),
        Variable("q")
      ),
      Implies(
        Variable("p"),
        Variable("r")
      )
    )
  )
)

let t2 = 
  proof [] f2
    |> intro "H1" 
    |> intro "H2"
    |> intro "H3"
    |> apply_assm "H1"
    |> apply_assm "H3"
    |> apply_assm "H2"
    |> apply_assm "H3"
  |> qed

let f3 = (
  Implies(
    Implies(
      Implies(
        Implies(
          Variable("p"),
          Bottom
        ),
        Variable("p")
      ),
      Variable("p")
    ),
    Implies(
      Implies(
        Implies(
          Variable("p"),
          Bottom
        ),
        Bottom
      ),
      Variable("p")
    )
  )
)

let t3 = proof [] f3
  |> intro "H1"
  |> intro "H2"
  |> apply_assm "H1"
  |> intro "H3"
  |> apply_assm "H2"
  |> apply_assm "H3"
|> qed;;

let f4 = (
  Implies(
    Implies(
      Implies(
        Implies(
          Variable("p"),
          Bottom
        ),
        Bottom
      ),
      Variable("p")
    ),
    Implies(
      Implies(
        Implies(
          Variable("p"),
          Bottom
        ),
        Variable("p")
      ),
      Variable("p")
    )
  )
)

let t4 = proof [] f4
  |> intro "H1"
  |> intro "H2"
  |> apply_assm "H1"
  |> intro "H3"
  |> apply_assm "H3"
  |> apply_assm "H2"
  |> apply_assm "H3"
|> qed;;
