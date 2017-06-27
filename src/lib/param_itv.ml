module PB = Param_bound
type t = {low : PB.t ; up : PB.t }

let singleton x = let z = PB.of_int x in {low=z; up=z}

let zero = singleton 0;;

let add x y =
  {low = PB.add x.low y.low; up = PB.add x.up y.up}

  


                                    
