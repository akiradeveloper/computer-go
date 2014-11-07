open Board
open Core.Std

let finish mat = assert false

(* Chinese rule *)
let count t = 
  let bw = Array.create 2 0 in
  for i = 1 to size t do
    for j = 1 to size t do
      match t.matrix.(i).(j) with
      | 2 -> assert false
      | 3 -> assert true
      | _ as a -> bw.(a) <- bw.(a) + 1
    done
  done
