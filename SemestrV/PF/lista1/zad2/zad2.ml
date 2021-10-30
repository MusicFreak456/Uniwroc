let f x = invalid_arg "ta da?"

let rec f x = f x

(* Generalnie chodzi o to żeby funkcja nigdy się nie kończyła *)