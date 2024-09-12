
open Assign01_02
let to_string s =
  let rec factorize s prime exponents =
    if s = 1 then List.rev exponents
    else
      let rec count_divisions s prime power =
        if s mod prime = 0 then count_divisions (s / prime) prime (power + 1)
        else power
      in
      let power = count_divisions s prime 0 in
      if power > 0 then
        factorize (s / (int_of_float (float_of_int prime ** float_of_int power))) (nth_prime (List.length exponents + 1)) (power :: exponents)
      else
        factorize s (nth_prime (List.length exponents + 1)) exponents
  in
  let exponents = factorize s (nth_prime 0) [] in
  let filtered_exponents = List.filter (fun x -> x > 0) exponents in
  if List.length filtered_exponents = 0 then "[]"
  else
    let formatted = String.concat "; " (List.map string_of_int filtered_exponents) in
    "[" ^ formatted ^ "]"
