(* Helper function to compute the lifespan of a function f with respect to start and pred *)
let rec lifespan f s pred =
  if pred s then 0
  else 1 + lifespan f (f s) pred

(* Main function to determine the last function standing *)
let last_function_standing funcs start pred =
  match funcs with
  | [] -> None  (* If no functions, return None *)
  | _ ->
    (* Compute lifespan for each function *)
    let lifespans = List.map (fun f -> (f, lifespan f start pred)) funcs in
    (* Sort by lifespan in descending order *)
    let sorted = List.sort (fun (_, l1) (_, l2) -> compare l2 l1) lifespans in
    match sorted with
    | (f, max_lifespan) :: (f2, lifespan2) :: _ when max_lifespan = lifespan2 -> None  (* Multiple with same lifespan *)
    | (f, max_lifespan) :: _ -> Some f  (* Return the function with the unique longest lifespan *)
    | _ -> None  (* Fallback, shouldn't be reached *)
