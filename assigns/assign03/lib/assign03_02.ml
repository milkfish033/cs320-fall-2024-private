let gen_fib l k =
  let len_l = List.length l in
    let rec aux acc n =
      if n < len_l then List.nth l n
      else match acc with
        | [] -> failwith "Accumulator should never be empty"
        | _ :: tail ->
          let new_value = List.fold_left (+) 0 acc in
          if n = k then new_value
          else aux (tail @ [new_value]) (n + 1)
    in aux l len_l
