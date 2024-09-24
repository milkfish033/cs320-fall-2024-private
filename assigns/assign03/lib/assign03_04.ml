let rec is_valid = function
  | [] | [_] -> true
  | x :: 0 :: y :: rest ->
      if (x > 0 && y < 0) || (x < 0 && y > 0) then is_valid (y :: rest)
      else false
  | x :: y :: rest ->
      if (x > 0 && (y > 0 || y = 0)) || (x < 0 && (y < 0 || y = 0)) then is_valid (y :: rest)
      else false

(* Helper function to group a valid list *)
let rec group_valid lst current acc =
  match lst with
  | [] -> List.rev (List.rev current :: acc)
  | 0 :: rest when current = [] -> group_valid rest current acc
  | 0 :: rest -> group_valid rest [] (List.rev current :: acc)
  | x :: rest -> group_valid rest (x :: current) acc

(* Main function *)
let group lst =
  if is_valid lst then
    Some (group_valid lst [] [])
  else
    None