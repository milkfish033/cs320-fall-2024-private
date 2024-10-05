

type set_info = {
  ind : int -> bool;
  mn : int;
  mx : int;
}

module ListSet = struct
  type t = int list

  let empty = []

  let mem x s = List.mem x s

  let rec add x s =
    match s with
    | [] -> [x]
    | hd :: tl ->
        if x = hd then s
        else if x < hd then x :: s
        else hd :: (add x tl)

  let rec remove x s =
    match s with
    | [] -> []
    | hd :: tl ->
        if x = hd then tl
        else hd :: (remove x tl)

  let rec union s1 s2 =
    match s1, s2 with
    | [], s | s, [] -> s
    | x1 :: xs1, x2 :: xs2 ->
        if x1 = x2 then x1 :: union xs1 xs2
        else if x1 < x2 then x1 :: union xs1 s2
        else x2 :: union s1 xs2

  let rec inter s1 s2 =
    match s1, s2 with
    | [], _ | _, [] -> []
    | x1 :: xs1, x2 :: xs2 ->
        if x1 = x2 then x1 :: inter xs1 xs2
        else if x1 < x2 then inter xs1 s2
        else inter s1 xs2

  let elements s = s
end

module FuncSet = struct
  type t = set_info

  let empty = { ind = (fun _ -> false); mn = 1; mx = 0 }

  let mem x s = s.ind x

  let add x s =
    let new_mn = if mem x s then s.mn else min s.mn x in
    let new_mx = if mem x s then s.mx else max s.mx x in
    { ind = (fun y -> s.ind y || x = y); mn = new_mn; mx = new_mx }

  let remove x s =
    if not (mem x s) then s
    else
      let new_ind y = if y = x then false else s.ind y in
      (* Find new bounds by checking all values in the range [mn..mx] *)
      let new_mn, new_mx =
        let rec find_bounds i mn mx =
          if i > s.mx then (mn, mx)
          else if new_ind i then
            let new_mn = min mn i in
            let new_mx = max mx i in
            find_bounds (i + 1) new_mn new_mx
          else find_bounds (i + 1) mn mx
        in find_bounds s.mn s.mx s.mn
      in { ind = new_ind; mn = new_mn; mx = new_mx }

  let union s1 s2 =
    let new_ind x = s1.ind x || s2.ind x in
    let new_mn = min s1.mn s2.mn in
    let new_mx = max s1.mx s2.mx in
    { ind = new_ind; mn = new_mn; mx = new_mx }

  let inter s1 s2 =
    let new_ind x = s1.ind x && s2.ind x in
    (* Recalculate new bounds after intersection *)
    let rec find_bounds i mn mx =
      if i > s1.mx then (mn, mx)
      else if new_ind i then find_bounds (i + 1) (min mn i) (max mx i)
      else find_bounds (i + 1) mn mx
    in
    let new_mn, new_mx = find_bounds s1.mn s1.mx s1.mx in
    if new_mn > new_mx then empty
    else { ind = new_ind; mn = new_mn; mx = new_mx }

  let elements s =
    let rec aux i acc =
      if i > s.mx then acc
      else if s.ind i then aux (i + 1) (i :: acc)
      else aux (i + 1) acc
    in aux s.mn []
end
