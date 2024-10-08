

type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

module ListSet = struct
  (* Define the type t as int list representing sorted distinct integers *)
  type t = int list

  (* The empty set is represented by an empty list *)
  let empty = []

  (* Create a singleton set containing just one element n *)
  let singleton n = [n]

  (* Check if an element x is a member of the set *)
  let  mem x s = 
    let rec check a l = 
      match l with 
        | [] -> false
        | h :: t -> if a = h then true else if a < h then false else check x t
    in check x s

  (* Calculate the cardinality of the set (i.e., the length of the list) *)
  let card s = List.length s

  (* Union of two sorted lists without duplicates *)
  let rec union s1 s2 = 
    match s1, s2 with
    | [], _ -> s2
    | _, [] -> s1
    | h1 :: t1, h2 :: t2 ->
      if h1 = h2 then h1 :: union t1 t2
      else if h1 < h2 then h1 :: union t1 s2
      else h2 :: union s1 t2
end


module FuncSet = struct
    type t = set_info
  
    let empty = {
      ind = (fun _ -> false);
      mn = 1;
      mx = 0;
    }
  
    let singleton n = {
      ind = (fun x -> x = n);
      mn = n;
      mx = n;
    }
  
    let mem x s = s.ind x
  
    let card s =
      let rec count acc i =
        if i > s.mx then acc
        else if s.ind i then count (acc + 1) (i + 1)
        else count acc (i + 1)
      in
      count 0 s.mn
  
    let union s1 s2 =
      let new_ind x = s1.ind x || s2.ind x in
      let new_mn = min s1.mn s2.mn in
      let new_mx = max s1.mx s2.mx in
      {
        ind = new_ind;
        mn = new_mn;
        mx = new_mx;
      }
end  