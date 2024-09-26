(* Tree definition *)
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

(* Helper function to extract all terminal elements from a tree *)
let rec terminal_elements t =
  match t with
  | Leaf _ -> [t]
  | Node children -> 
    if children = [] then [t]
    else List.concat (List.map terminal_elements children)

(* Collapse function *)
let rec collapse h t =
    match t with
    | Leaf _ -> t  (* A leaf stays the same regardless of the collapse *)
    | Node children ->
      if height t < h then t  (* No need to collapse if the tree is already shorter than h *)
      else if h = 1 then Node (terminal_elements t)  (* Collapse to terminal elements *)
      else Node (List.map (collapse (h - 1)) children)  (* Recursively collapse subtrees *)
