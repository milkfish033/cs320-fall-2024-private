let mk_unique_keys  alst =
  let rec update_assoc key value lst = match lst with
    | [] -> [(key, value)]  (* since acc is initialized as an empty list, add the first key, value to it *)
    | (k, v) :: tail ->
        if k = key then (k, v + value) :: tail  (* If key is found, update its value *)
        else (k, v) :: (update_assoc key value tail)  (* Recur to check rest of the list *)
  in
  List.fold_left (fun acc (key, value) -> update_assoc key value acc) [] alst
