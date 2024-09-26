let gen_fib l k =
  let len = List.length l in
  if len = 0 || k < 0 then failwith "Invalid input"
  else
    let rec aux n acc =
      if n < len then List.nth l n
      else
        let sum = List.fold_left (fun acc i -> acc + (aux (n - i) acc)) 0 (List.init len (fun x -> x + 1)) in
        sum
    in
    aux k 0
