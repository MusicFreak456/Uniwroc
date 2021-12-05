type _ expr =
| Const : 'a -> 'a expr
| App   : ('a -> 'b) expr * 'a expr -> 'b expr

let rec eval : type a. a expr -> a =
  function
  | Const x   -> x
  | App(f, a) -> (eval f) (eval a)

type _ tp =
| Int : int tp
| Arr : 'a tp * 'b tp -> ('a -> 'b) tp

type tr_result =
| Ok : 'a expr * 'a tp -> tr_result

type (_, _) eq =
| Refl : ('a, 'a) eq

let rec type_eq : type a b. a tp -> b tp -> (a, b) eq option =
  fun t1 t2 ->
  match t1, t2 with
  | Int, Int -> Some Refl
  | Int, _   -> None
  | Arr(ta1, tv1), Arr(ta2, tv2) ->
    begin match type_eq ta1 ta2, type_eq tv1 tv2 with
    | Some Refl, Some Refl -> Some Refl
    | _ -> None
    end
  | Arr _, _ -> None

let rec tr : UExpr.t -> tr_result =
  function
  | UInt   n   -> Ok(Const n, Int)
  | UConst "+" -> Ok(Const (+), Arr(Int, Arr(Int, Int)))
  | UConst _   -> failwith "undefined op"
  | UApp(e1, e2) ->
    begin match tr e1 with
    | Ok(e1, Arr(t2, t1)) ->
      let (Ok(e2, t2')) = tr e2 in
      begin match type_eq t2 t2' with
      | None -> failwith "type error"
      | Some Refl -> Ok(App(e1, e2), t1)
      end
    | Ok(_, _) -> failwith "type error"
    end

let run_int_expr : string -> int =
  fun str ->
  match tr (UExpr.parse str) with
  | Ok(e, Int) -> eval e
  | _ -> failwith "type error"
