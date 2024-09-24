type tree =
  | Leaf of int
  | Node of tree list

(* Helper function to compute the height of a tree *)
let rec height t =
  match t with
  | Leaf _ -> 0
  | Node children ->
      if children = [] then 0
      else 1 + List.fold_left (fun acc child -> max acc (height child)) 0 children

(* Helper function to gather all terminal elements (leaves or empty nodes) of a subtree *)
let rec terminal_elements t =
  match t with
  | Leaf _ -> [t]
  | Node children ->
      if children = [] then [t]
      else List.concat (List.map terminal_elements children)

(* The collapse function *)
let rec collapse h t =
  if h = 0 then t
  else match t with
    | Leaf _ -> t
    | Node children ->
        if height t <= h then t
        else Node (List.concat (List.map (fun child ->
          if height child = h - 1 then terminal_elements child
          else [collapse h child]) children))
