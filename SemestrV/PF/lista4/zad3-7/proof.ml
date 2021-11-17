open Logic

type goal = (string * formula) list * formula 

type incomplete_proof =
| Goal       of goal
| P_Complete of theorem
| ImplI      of formula * incomplete_proof
| ImplE      of incomplete_proof * incomplete_proof
| BottE      of formula * incomplete_proof 

type context =
| Root
| ImplI_Ctx   of formula * context
| ImplE_L_Ctx of context * incomplete_proof
| ImplE_R_Ctx of incomplete_proof * context
| BottE_Ctx   of formula * context

type proof =
| Complete   of theorem
| Incomplete of goal * context

let proof g f =
  Incomplete((g,f), Root)

let qed pf =
  match pf with
  | Complete(t) -> t
  | Incomplete(_,_) -> failwith "Proof incomplete"

let goal pf =
  match pf with
  | Complete(_) -> None
  | Incomplete(g,_) -> Some(g) 

let rec search_up ctx ip =
  match ip with
  | Goal(g)           -> Some( Incomplete(g, ctx) )
  | P_Complete(_)     -> None
  | ImplI(f,ip)       -> search_up (ImplI_Ctx(f,ctx)) ip
  | BottE(f,ip)       -> search_up (BottE_Ctx(f,ctx)) ip
  | ImplE(ip_l, ip_r) ->
    match search_up (ImplE_L_Ctx(ctx, ip_r)) ip_l with
    | None -> search_up (ImplE_R_Ctx(ip_l, ctx)) ip_r
    | some -> some

let rec search_down ctx ip = 
  match ctx with
  | Root           -> None
  | ImplI_Ctx(f, ctx) -> search_down ctx (ImplI(f,ip))
  | BottE_Ctx(f, ctx) -> search_down ctx (BottE(f,ip))
  | ImplE_L_Ctx(ctx, ip_r) ->
    begin match search_up (ImplE_R_Ctx(ip, ctx)) ip_r with
    | None -> search_down ctx (ImplE(ip, ip_r))
    | some -> some
    end
  | ImplE_R_Ctx(ip_l, ctx) -> 
    begin match search_down ctx (ImplE(ip_l, ip)) with
    | None -> search_up (ImplE_L_Ctx(ctx, ip)) ip_l
    | some -> some 
    end
  
let next pf =
  match pf with
  | Complete(_) -> pf
  | Incomplete(g, ctx) ->
    begin match search_down ctx (Goal g) with
    | None -> pf
    | Some(g) -> g
    end

let intro name pf =
  match pf with
  | Complete(_) -> failwith "Proof is completed"
  | Incomplete((assm, f), ctx) ->
    match f with
    | Bottom | Variable(_) -> failwith "Targeted goal is not implication"
    | Implies(p,q) -> 
      Incomplete( (List.cons (name, p) assm, q), (ImplI_Ctx(p,ctx)) )

let fill_imple_bott f pf =
  match pf with
  | Complete(_) -> failwith "Cannot apply apply to incomplete proof"
  | Incomplete( (assm, g), ctx ) ->
    let base = (Goal(assm, f)) in
    let rec eliminate f base = 
      begin match f with
      | f when Stdlib.compare f g = 0 -> base
      | Bottom -> BottE(g,base)
      | Implies(p, q) -> eliminate q (ImplE(base, Goal(assm, p)))
      | _ -> failwith "Bad apply usage"
      end 
    in 
    match search_up ctx (eliminate f base) with
    | None -> assert false
    | Some(p) -> p

let rec search_down_with_invariant ip ctx =
  match ip with
  | P_Complete(thm) -> 
    begin match ctx with
    | Root -> Some(Complete(thm))
    | ImplI_Ctx(f, ctx) -> 
      search_down_with_invariant (P_Complete(imp_i f thm)) ctx
    | BottE_Ctx(f, ctx) ->
      search_down_with_invariant (P_Complete(bot_e f thm)) ctx
    | ImplE_L_Ctx(ctx_i, ip_i) ->
      begin match ip_i with
      | P_Complete(thm_r) ->
        search_down_with_invariant (P_Complete(imp_e thm thm_r)) ctx_i
      | _ -> search_down ctx ip
      end
    | ImplE_R_Ctx(ip_i, ctx_i) -> 
      begin match ip_i with
      | P_Complete(thm_l) ->
        search_down_with_invariant (P_Complete(imp_e thm_l thm)) ctx_i
      | _ -> search_down ctx ip
      end
    end
  | _ -> search_down ctx ip

let check_assm thm assm =
  let assm = List.map (fun (name, f) -> f) assm 
  and thm_assm = assumptions thm in
  let mem_vec = List.map (fun x -> List.mem x assm) thm_assm in
  List.fold_left (fun acc x -> (acc && x)) true mem_vec

let fill_thm thm pf =
  match pf with
  | Complete(_) -> assert false
  | Incomplete( (assm, g), ctx) ->
    if check_assm thm assm then
    search_down_with_invariant (P_Complete(thm)) ctx
    else failwith "Theorem requires additional assumptions"
  
let apply f pf = fill_imple_bott f pf

let apply_thm thm pf =
  let new_active = apply (consequence thm) pf in
  match fill_thm thm new_active with
  | None -> assert false
  | Some(p) -> p

let apply_assm name pf =
  match pf with 
  | Complete(_) -> assert false
  | Incomplete( (assm,_), _) ->
    apply_thm (by_assumption (List.assoc name assm)) pf


let pp_print_proof fmtr pf =
  match goal pf with
  | None -> Format.pp_print_string fmtr "No more subgoals"
  | Some(g, f) ->
    Format.pp_open_vbox fmtr (-100);
    g |> List.iter (fun (name, f) ->
      Format.pp_print_cut fmtr ();
      Format.pp_open_hbox fmtr ();
      Format.pp_print_string fmtr name;
      Format.pp_print_string fmtr ":";
      Format.pp_print_space fmtr ();
      pp_print_formula fmtr f;
      Format.pp_close_box fmtr ());
    Format.pp_print_cut fmtr ();
    Format.pp_print_string fmtr (String.make 40 '=');
    Format.pp_print_cut fmtr ();
    pp_print_formula fmtr f;
    Format.pp_close_box fmtr ()