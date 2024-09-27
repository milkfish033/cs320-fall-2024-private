let sum_last_n lst n =
    let last_n_elements = List.rev (List.take n (List.rev lst)) in
    List.fold_left (+) 0 last_n_elements

let gen_fib l k = 
  let rec helper lst kth n acc =
    if kth < List.length l then List.nth l kth  
    else if n = kth then List.nth acc (List.length acc - 1) 
    else 
      let new_value = sum_last_n acc (List.length l) in
      helper lst kth (n + 1) (acc @ [new_value] )
  in
  helper l k (List.length l - 1) l 
