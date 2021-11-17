type 'a context = 'a list
type 'a zlist = 'a context * 'a list

let of_list list = ([], list)

let to_list (rev_list, list) = 
  List.rev_append rev_list list

let elem =
  function
  | (_,[])   -> None
  | (_,x::_) -> Some(x)

let move_left (ctx, list) =
  match ctx with
  | []     -> failwith "Out of bounds"
  | x::ctx -> ( ctx , x::list )

let move_right (ctx, list) =
  match list with
  | []    -> failwith "Out of bounds"
  | x::xs -> ( x::ctx, xs )

let insert x (ctx,list) = (x :: ctx, list)

let remove (ctx, list) = 
  match ctx with
  | [] -> failwith "No items to remove"
  | x::ctx -> (ctx, list)

let _ = assert ([] |> of_list |> to_list = [])
let _ = assert ([1] |> of_list |> to_list = [1])
let _ = assert ([1; 2] |> of_list |> to_list = [1; 2])

let zlist = of_list [1; 2; 3; 4; 5; 6; 7]
let _ = assert (zlist |> to_list = [1; 2; 3; 4; 5; 6; 7])
let _ = assert (zlist |> insert 0 |> to_list = [0; 1; 2; 3; 4; 5; 6; 7])

let _ =
  assert (zlist |> move_right |> insert 0 |> to_list = [1; 0; 2; 3; 4; 5; 6; 7])

let _ =
  assert (
    zlist |> move_right |> move_right |> insert 0 |> to_list
    = [1; 2; 0; 3; 4; 5; 6; 7])

let _ =
  assert (
    zlist |> move_right |> move_right |> move_left |> insert 0 |> to_list
    = [1; 0; 2; 3; 4; 5; 6; 7])

let _ =
  assert (
    zlist |> move_right |> move_right |> move_left |> move_right |> insert 0
    |> to_list = [1; 2; 0; 3; 4; 5; 6; 7])

let _ =
  assert (
    zlist |> insert 8 |> move_right |> insert 9 |> move_left |> insert 10
    |> to_list
    = [8; 1; 10; 9; 2; 3; 4; 5; 6; 7])

let _ = assert (zlist |> elem = Some 1)
let _ = assert (zlist |> move_right |> elem = Some 2)

let _ =
  assert (
    zlist |> move_right |> move_right |> move_right |> move_right |> move_right
    |> move_right |> elem = Some 7)

let _ =
  assert (
    zlist |> move_right |> move_right |> move_right |> move_right |> move_right
    |> move_right |> move_right |> elem = None)