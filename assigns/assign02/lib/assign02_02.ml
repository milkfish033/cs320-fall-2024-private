type matrix = {
entries : float list list;
rows : int;
cols : int;
}
(*helper function that generate the sub list*)
let rec split_list lst n =
  match lst with
  | [] -> []
  | _ ->
    let row = List.take n lst in
    let rest = List.drop n lst in
    row :: split_list rest n


let mk_matrix e (r, c) = {
  entries = split_list e r; rows = r; cols =c}
