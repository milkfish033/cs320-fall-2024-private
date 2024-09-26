let init n f =
  let rec aux i acc =
    if i < 0 then acc
    else aux (i - 1) (f i :: acc)
  in
  aux (n - 1) []
;;

let gen_fib l k =
  let len = List.length l in
  if len = 0 || k < 0 then failwith "Invalid input"
  else
    let rec aux n =
      if n < len then List.nth l n
      else
        let sum = List.fold_left (fun acc i -> acc + (aux (n - i))) 0 (init len (fun x -> x + 1)) in
        sum
    in
    aux k
