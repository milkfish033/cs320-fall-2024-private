let rec take n lst =
  match lst, n with
  | [], _ -> []
  | _, 0 -> []
  | x :: xs, n -> x :: (take (n - 1) xs)

(* Function to generate the generalized Fibonacci sequence *)
let gen_fib l k =
  (* Helper function to compute the next element in the sequence *)
  let rec aux seq k =
    if k < List.length l then List.nth l k  (* If k is within the starting values, return the corresponding element *)
    else
      let sum = List.fold_left (+) 0 (take (List.length l) seq) in
      aux (sum :: seq) (k - 1)
  in
  aux (List.rev l) k  (* Start the sequence with reversed initial values *)