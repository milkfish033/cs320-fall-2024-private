
type 'a test = 
| TestCase of 'a
| TestList of 'a test list


let rec fold_left op base test = 
    match test with 
      | TestCase x -> op base x
      | TestList cases -> 
          List.fold_left (fun acc t -> fold_left op acc t) base cases