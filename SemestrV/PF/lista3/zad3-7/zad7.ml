open Logic;;
(* #install_printer Logic.pp_print_formula;;
#install_printer Logic.pp_print_theorem;; *)

let p = Variable("p")
let q = Variable("q")
let bot = Bottom

let pdpkt_1 = imp_i p (by_assumption p)

let pdpkt_2 = imp_i p (imp_i q (by_assumption p))

let asm1 = Implies( Variable("p"), Implies(Variable("q"), Variable("r")) )
let asm2 = Implies( Variable("p"), Variable("q") )
let asm3 = Variable("p")
let thm1 = by_assumption asm1
let thm2 = by_assumption asm2
let thm3 = by_assumption asm3
let thm4 = (imp_e (imp_e thm1 thm3) (imp_e thm2 thm3))

let pdpkt_3 = (imp_i asm1 (imp_i asm2 (imp_i asm3 thm4)))

let pdpkt_4 = imp_i bot (bot_e p (by_assumption bot))