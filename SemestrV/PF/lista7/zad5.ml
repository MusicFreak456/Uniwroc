module BT : sig
  type 'a t

  val return : 'a -> 'a t
  val bind   : 'a t -> ('a -> 'b t) -> 'b t

  val fail : 'a t
  val flip : bool t

  val run : 'a t -> 'a Seq.t
end = struct
  type 'a t = 'a Seq.t

  let return x = List.to_seq [ x ]
  let rec bind m f = Seq.flat_map f m

  let fail = Seq.empty
  let flip = List.to_seq [ true; false ]

  let run m = m
end

type 'a regexp =
| Eps
| Lit  of ('a -> bool)
| Or   of 'a regexp * 'a regexp
| Cat  of 'a regexp * 'a regexp
| Star of 'a regexp

let ( or ) r1 r2 = Or(r1, r2)
let ( ^^ ) r1 r2 = Cat(r1, r2)
let (let*) = BT.bind

let fork a b =
  let* choice = BT.flip in
  if choice then a else b

let always_some str m = 
  let* x = m in
  begin match x with
  | None     -> BT.return (Some str)
  | Some str -> BT.return (Some str)
  end

let rec match_regexp : 'a regexp -> 'a list -> 'a list option BT.t =
  fun reg str ->
    match reg with
    | Eps      -> BT.return None
    | Lit pred -> 
      begin match str with
      | [] -> BT.fail
      | x::xs -> 
        if pred x 
          then BT.return (Some xs)
          else BT.fail
      end
    | Or(l,r) -> fork (match_regexp r str) (match_regexp l str)
    | Cat(l,r) ->
      let* r_str = match_regexp l str in
      begin match r_str with
      | None     -> match_regexp r str
      | Some str -> always_some str (match_regexp r str)
      end
    | Star re -> fork (BT.return None) begin
        let* res = match_regexp re str in
        begin match res with
        | None     -> BT.fail
        | Some str -> always_some str (match_regexp (Star re) str)
        end
      end

let re1 = Star (Star (Lit (( <> ) 'b')) or (Lit (( = ) 'b') ^^ Lit (( = ) 'a')))
(*               ( [^b]* | ba )*                       *)

let a = Lit (( = ) 'a')
let b = Lit (( = ) 'b')

let re2 = Star(Lit ((=)'a')) ^^ Lit ((=) 'b')

let test str re =
  match_regexp re (str |> String.to_seq |> List.of_seq) |> BT.run |> List.of_seq