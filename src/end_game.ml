open Core.Std

let finish mat = assert false

(* Chinese rule *)
let count mat = 
  let bw = Array.create 2 0 in
  let sz = (Array.length mat) - 2 in
  for i = 1 to sz do
    for j = 1 to sz do
      match mat.(i).(j) with
      | 2 -> assert false
      | 3 -> assert true
      | _ as a -> bw.(a) <- bw.(a) + 1
    done
  done
