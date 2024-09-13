type piece = 
  | X
  | O

type pos = 
  | Piece of piece
  | Blank

type board = (pos * pos * pos) * (pos * pos * pos) * (pos * pos * pos)

type row_index = 
  | Top
  | Middle
  | Bottom

type col_index = 
  | Left
  | Middle
  | Right

type pos_index = row_index * col_index



let get_pos b p = 
  let (x,y)=p in
  let ((t1,t2,t3),(m1,m2,m3),(b1,b2,b3)) =b  in 
  if (x,y) = (Top,Left) then t1
  else if (x,y) = (Top, Middle) then t2
  else if (x,y) = (Top, Right) then t3
  else if (x,y) = (Middle, Left) then m1
  else if (x,y) = (Middle, Middle) then m2
  else if (x,y) = (Middle, Right) then m3
  else if (x,y) = (Bottom, Left) then b1
  else if (x,y) = (Bottom, Middle) then b2
  else b3


  let winner b =
    let ((tl, tm, tr), (ml, mm, mr), (bl, bm, br)) = b in
    let is_winning_line p1 p2 p3 =
      p1 = p2 && p2 = p3 && p1 <> Blank
    in
    is_winning_line tl tm tr || 
    is_winning_line ml mm mr ||
    is_winning_line bl bm br ||
    is_winning_line tl ml bl ||
    is_winning_line tm mm bm ||
    is_winning_line tr mr br ||
    is_winning_line tl mm br ||
    is_winning_line tr mm bl
  
 


