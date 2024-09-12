open Assign01_02
let nth s i =
  let rec find_exponent s prime count =
    if count = i then
      let rec count_divisions s prime power =
        if s mod prime = 0 then count_divisions (s / prime) prime (power + 1)
        else power
      in
      count_divisions s prime 0
    else if s mod prime = 0 then
      find_exponent s (nth_prime (count + 1)) (count + 1)
    else
      find_exponent s (nth_prime (count + 1)) (count + 1)
  in
  find_exponent s (nth_prime 0) 0 ;;
