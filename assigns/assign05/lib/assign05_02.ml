
type 'a tree =
  | Leaf
  | Node of 'a * 'a tree * 'a tree

  let sum_tr t =
    let rec go t cont =
      match t with
      | Leaf -> cont 0  (* If the tree is empty, call the continuation with 0 *)
      | Node (x, l, r) -> 
          go l (fun sum_l -> (* Recur on the left child, passing a continuation *)
            go r (fun sum_r -> (* Recur on the right child with another continuation *)
              cont (x + sum_l + sum_r)  (* Call continuation with the accumulated sum *)
            )
          )
    in
    go t (fun x -> x)  (* Start with the identity continuation *)
  