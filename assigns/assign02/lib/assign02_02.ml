type matrix = {
entries : float list list;
rows : int;
cols : int;
}

let mk_matrix e (r, c) = 
  