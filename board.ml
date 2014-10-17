open Array
type t = int array array

(*
 * black = 0
 * while = 1
 * fridge = 2
 * empty = 3
 *)
let make n =
  let b = make_matrix (n+2) (n+2) 2 in
  for i = 1 to n do
    for j = 1 to n do
      b.(i).(j) <- 3
    done
  done ;
  b

let flip_color = function
  | 0 -> 1
  | 1 -> 0
  | _ -> assert false

let show b = 
  Printf.printf "   [1 2 3 4 5 6 7 8 9 10111213141516171819]\n" ;
  let p i = match i with
  | 0 -> '@'
  | 1 -> 'O'
  | 3 -> ' '
  | _ -> assert false
  in
  for i = 1 to (length b) - 2 do
    Printf.printf "%2d| " i ;
    for j = 1 to (length b) - 2 do
      (* Printf.printf "%c " (p (get (get b i) j)) ; *)
      Printf.printf "%c " @@ p b.(i).(j) ;
    done ;
    print_newline () ;
  done

(* let put_stone t (i, j) a = set (get t i) j a *)
(* ;; *)
let put_stone t (i, j) a =
  t.(i).(j) <- a

(* do_put_stones can be implemented in either way *)
(* let rec do_put_stones t xs = *)
(*   match xs with *)
(*   | [] -> () *)
(*   | (i, j, a) :: xs' -> *)
(*     put_stone t (i, j) a ; *)
(*     Printf.printf "(%d,%d,%d)\n" i j a ; *)
(*     do_put_stones t xs' *)
(* ;; *)
let do_put_stones t xs =
  List.iter (fun (i, j, a) -> put_stone t (i, j) a) xs

let put_stones t xs =
  let rec zip xs ys =
    match (xs, ys) with
    | ([], _) -> []
    | (_, []) -> []
    | ((i,j) :: xs', a :: ys') -> (i, j, a) :: (zip xs' ys') in
  let alt_color xs = zip xs (List.mapi (fun i _ -> i mod 2) xs) in
  do_put_stones t @@ alt_color xs

let out_board t = function
  | (0, _) | (20, _) | (_, 0) | (_, 20) -> false
  | _ -> true

let pos2int (i, j) = (i lsl 5) + j
let int2pos n = (n lsr 5, n land 31)

let search_hole t (i, j) =
  if t.(i+1).(j) == 3 then true else
    if t.(i-1).(j) == 3 then true else
      if t.(i).(j+1) == 3 then true else
        if t.(i).(j-1) == 3 then true else false

module IntSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = int
  end)
;;

(* TODO *)
let do_remove_stones b (i, j, color) =
  let poss = ref [] in
  let s = ref IntSet.empty in
  let found_hole = ref false in
  let rec visit (i, j, a) =
    if a != color then ()
    else
      if out_board b (i, j) then ()
      else
        if !found_hole then ()
        else
          if search_hole b (i, j) then
            found_hole := true 
          else
            if IntSet.mem (pos2int (i, j)) !s then
              poss := (i, j) :: !poss
            else
              s := IntSet.add (pos2int (i, j)) !s ;
              visit (i+1, j, flip_color a) ;
              visit (i-1, j, flip_color a) ;
              visit (i, j+1, flip_color a) ;
              visit (i, j-1, flip_color a) ;
  in
  visit (i, j, color) ;
  if !found_hole then [] else !poss

let can_put t (i, j) = assert false

let b = make 19 ;;

(* let l = [(4,4); (16,16); (17,14); (14,16); (16,10)] ;; *)
(* put_stones b l ; *)

let l = [(10,10,0); (10,9,1); (10,11,1); (9,10,1); (11,10,1)] ;;
do_put_stones b l ;
do_remove_stones b (10,10,0) ;
show b ;
