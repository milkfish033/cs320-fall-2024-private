
let lifespan f start pred =
  let rec aux s steps =
    if pred s then steps
    else aux (f s) (steps + 1)
  in
  aux start 0

let last_function_standing funcs start pred =
  let rec find_best funcs best_func best_lifespan is_tied =
    match funcs with
    | [] -> if is_tied then None else best_func
    | f :: fs ->
      let f_lifespan = lifespan f start pred in
      if f_lifespan > best_lifespan then
        find_best fs (Some f) f_lifespan false
      else if f_lifespan = best_lifespan then
        find_best fs best_func best_lifespan true
      else
        find_best fs best_func best_lifespan is_tied
  in
  find_best funcs None (-1) false
