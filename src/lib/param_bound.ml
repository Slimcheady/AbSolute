
type t =
  | Binop of op * t * t
  | BoundInt of int
  | BoundParam of param 

 and op =
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
  | _ -> failwith "pas encore implementé"


  
