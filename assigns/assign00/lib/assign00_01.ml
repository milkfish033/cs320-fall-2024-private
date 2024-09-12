let sqrt n =
  let rec find_sqrt k =
    if k * k >= n then k
    else find_sqrt (k + 1)
  in
  find_sqrt 0 ;;
