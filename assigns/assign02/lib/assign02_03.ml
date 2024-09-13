type dir = 
| North
| South
| East
| West

type path = dir list

let dist dirs = 
  let rec calc_dis dirs(x,y) =
    match dirs with
    |[] -> (x,y) (*base case*)
    | North :: rest -> calc_dis rest (x,y+1)
    | South :: rest -> calc_dis rest (x,y-1)
    | West :: rest -> calc_dis rest (x-1,y)
    | East :: rest -> calc_dis rest (x+1,y)
  in let (final_x, final_y) = calc_dis dirs(0,0) in 
  sqrt(float_of_int(final_x * final_x + final_y * final_y))