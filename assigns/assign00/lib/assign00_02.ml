let is_prime n =
  let rec check_divisor d =
    if d * d > n then true
    else if n mod d = 0 then false
    else check_divisor (d + 1)
  in
  if n < 2 then false
  else check_divisor 2