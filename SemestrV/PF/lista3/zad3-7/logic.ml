type formula = 
| Bottom
| Variable of string
| Implies of formula * formula

let rec compare_formulas f1 f2 =
  match f1,f2 with
  | Bottom, Bottom -> 0
  | Bottom, _ -> -1
  | Variable(a), Variable(b) -> String.compare a b
  | Variable(_), Bottom -> 1
  | Variable(_), Implies (_,_) -> -1
  | Implies(l1, r1), Implies(l2,r2) ->  
    let l = compare_formulas l1 l2
    and r = compare_formulas r1 r2
    in if l = 0 then r else l
  | Implies(_,_), _ -> 1

let rec string_of_formula f =
  match f with 
  | Bottom -> "⊥"
  | Variable(s) -> s
  | Implies(left_formula, right_formula) ->
    let str_of_lformula = string_of_formula left_formula 
    and str_of_rformula = string_of_formula right_formula in
    begin match left_formula with
    | Implies(l, r) -> "(" ^ str_of_lformula ^ ")" ^ " ⟶ " ^ str_of_rformula
    | Bottom | Variable(_)  -> str_of_lformula ^ " ⟶ " ^ str_of_rformula
    end

let pp_print_formula fmtr f =
  Format.pp_print_string fmtr (string_of_formula f)

module F = struct
  type t = formula
  let compare = compare_formulas
end

module S = Set.Make(F)
(* type judgement = S.t * formula 
type theorem = 
| Ax       of judgement
| Impl_Int of judgement * theorem
| Impl_Elm of judgement * theorem * theorem
| Bott_Elm of judgement * theorem *)

type theorem = S.t * formula

(* let judgement thm =
  match thm with
  | Ax(j)           -> j
  | Impl_Int(j,_)   -> j
  | Impl_Elm(j,_,_) -> j
  | Bott_Elm(j,_)   -> j *)

let judgement thm = thm

let assumptions thm =
  let (s,_) = judgement thm 
  in S.fold (fun x acc -> List.cons x acc) s []

let consequence thm =
  let (_,f) = judgement thm 
  in f

let pp_print_theorem fmtr thm =
  let open Format in
  pp_open_hvbox fmtr 2;
  begin match assumptions thm with
  | [] -> ()
  | f :: fs ->
    pp_print_formula fmtr f;
    fs |> List.iter (fun f ->
      pp_print_string fmtr ",";
      pp_print_space fmtr ();
      pp_print_formula fmtr f);
    pp_print_space fmtr ()
  end;
  pp_open_hbox fmtr ();
  pp_print_string fmtr "⊢";
  pp_print_space fmtr ();
  pp_print_formula fmtr (consequence thm);
  pp_close_box fmtr ();
  pp_close_box fmtr ()

let by_assumption f =
  let assum = S.add f S.empty
  (* in Ax((assum,f)) *)
  in (assum, f)

let imp_i f thm =
  let (a,c) = judgement thm in
  let assum = S.remove f a 
  and concl  = Implies(f,c)
  (* in Impl_Int((assum, concl), thm) *)
  in (assum, concl)

let imp_e th1 th2 =
  let (a1, c1) = judgement th1
  and (a2, c2) = judgement th2 
  in match c1 with
  | Implies(l, r) when (compare_formulas l c2) = 0 -> 
      let assum = S.union a1 a2
      (* in Impl_Elm( (assum, r), th1, th2 )  *)
      in (assum, r)
  | Bottom | Variable(_) | Implies(_,_) -> failwith("Bad impl_e usage")

let bot_e f thm =
  let(a, c) = judgement thm
  in match c with
  (* | Bottom -> Bott_Elm((a,f),thm) *)
  | Bottom -> (a,f)
  | Variable(_) | Implies(_,_) -> failwith("Bad bot_e usage")
