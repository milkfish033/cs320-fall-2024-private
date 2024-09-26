(* Define a function that computes the generalized Fibonacci sequence *)
let gen_fib (l : int list) (k : int) : int =
  (* Check for edge cases *)
  let len = List.length l in
  if len = 0 || k < 0 then failwith "Invalid input: empty list or negative index" else

  (* Use an array to store previously computed values for efficiency *)
  let memo = Array.make (k + 1) 0 in

  (* Tail recursive helper function to compute G(n) with memoization *)
  let rec aux n =
    if n < len then
      List.nth l n  (* Base case: return the starting value for n less than len *)
    else if memo.(n) <> 0 then
      memo.(n)  (* Return cached result if already computed *)
    else begin
      (* Compute the generalized Fibonacci number *)
      let sum = ref 0 in
      for i = 1 to len do
        sum := !sum + (aux (n - i))  (* Accumulate G(n-i) *)
      done;
      memo.(n) <- !sum;  (* Store computed result in memo array *)
      !sum  (* Return the computed sum *)
    end
  in
  aux k  (* Call the helper function with k *)
