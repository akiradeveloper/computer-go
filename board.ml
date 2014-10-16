open Array
type t = int array array
let make n = make_matrix (n+2) (n+2) 2
let show b = 
  Printf.printf "   [1 2 3 4 5 6 7 8 9 10111213141516171819]\n" ;
  let p i = match i with
  | 0 -> '@'
  | 1 -> 'O'
  | _ -> ' '
  in
  for i = 1 to (length b) - 2 do
    Printf.printf "%2d| " i;
    for j = 1 to (length b) - 2 do
      Printf.printf "%c " (p (get (get b i) j)) ;
    done ;
    print_newline () ;
  done
let put_stone t (i, j) a = set (get t i) j a
let can_put t (i, j) = assert false

let b = make 19 ;;
put_stone b (4, 4) 0 ;
put_stone b (16, 16) 1 ;
put_stone b (17, 14) 0 ;
put_stone b (14, 16) 1 ;
put_stone b (16, 10) 0 ;
show b ;
