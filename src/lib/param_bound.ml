
type t =
  | Binop of bop * t * t
  | BoundInt of int
  | BoundParam of param
  | Neg of t

 and bop =
   | Plus
   | Minus
   | Min
   | Max

 and param = int ;;

let of_int (x:int) = BoundInt x 

(* On place les param a gauche, les cst a droite [Le + possible] *)
let rec add x y =
  match x,y with
  | (BoundInt a), (BoundInt b) -> BoundInt(a+b)
  | (BoundInt a), (BoundParam b) -> Binop (Plus,y,x)
  | (BoundInt a), (Binop (b,gauche,droite)) -> Binop (b,gauche, (add droite x))
  | (BoundInt a), (Neg (BoundInt b)) -> BoundInt(a-b)
  | (BoundInt a), (Neg (BoundParam b)) -> Binop(Plus,y,x)
  | (BoundParam a), (BoundParam b) -> Binop (Plus,x,y)
  | (BoundParam a), (BoundInt b) -> Binop (Plus,x,y)
  | (BoundParam a), (Binop (b,gauche,droite)) -> 
  | _ -> failwith "pas encore implementé"


  
