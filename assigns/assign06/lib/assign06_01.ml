
open Utils

  let rec helper l acc =
    match l with 
    | [] -> Some (List.rev acc)  (* Reverse `acc` to maintain original order *)
    | h :: t -> 
        match tok_of_string_opt h with
        | None -> None  (* Return `None` if token conversion fails *)
        | Some tok -> helper t (tok :: acc)  (* Prepend the token to `acc` and continue *)
  
  let lex l = 
    if l = "" then Some []  (* Return `None` if input string is empty *)
    else 
      let lst = split l in
      helper lst []  (* Start with an empty accumulator *)