module P = Perm.Make(Int)

let test_list = [1;2;3;4;5]

let id_perm = P.id

let swp_1_w_5 = P.swap 1 5

let swp_5_w_3 = P.swap 5 3

let swp_comp = P.compose swp_1_w_5 swp_5_w_3

let suppose_to_be_id = P.compose swp_1_w_5 (P.invert swp_1_w_5)


let _ = assert( List.fold_right 
                  (fun x acc -> P.apply id_perm x :: acc) 
                  test_list 
                  [] 
                = [1;2;3;4;5] )

let _ = assert( List.fold_right 
                (fun x acc -> P.apply swp_1_w_5 x :: acc) 
                test_list 
                [] 
              = [5;2;3;4;1] )

let _ = assert( List.fold_right 
                (fun x acc -> P.apply suppose_to_be_id x :: acc) 
                test_list 
                [] 
              = [1;2;3;4;5] )

let _ = assert( List.fold_right 
                (fun x acc -> P.apply (P.invert suppose_to_be_id) x :: acc) 
                test_list 
                [] 
              = [1;2;3;4;5] )

let _ = assert( List.fold_right 
                (fun x acc -> P.apply swp_comp x :: acc) 
                test_list 
                [] 
              = [5;2;1;4;3] )

let _ = assert( List.fold_right 
                (fun x acc -> P.apply (P.invert swp_comp) x :: acc) 
                test_list 
                [] 
              = [3;2;5;4;1] )

let _ = assert( List.fold_right 
                (fun x acc -> P.apply (P.compose swp_comp (P.invert swp_comp)) x :: acc) 
                test_list 
                [] 
              = [1;2;3;4;5] )

let _ = assert( P.compare swp_1_w_5 swp_5_w_3 <> 0 )

let _ = assert( P.compare swp_1_w_5 swp_1_w_5 = 0 )