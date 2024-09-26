let rec gen_fib l k= 
  if k < List.length l then List.nth l k 
  else gen_fib l (k-1) + gen_fib l (k-2)