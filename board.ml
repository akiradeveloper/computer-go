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
  | (0, _) | (20, _) | (_, 0) | (_, 20) -> true
  | _ -> false

let pos2int (i, j) = (i lsl 5) + j
let int2pos n = (n lsr 5, n land 31)

let search_hole t (i, j) =
  if t.(i+1).(j) == 3 then true else
    if t.(i-1).(j) == 3 then true else
      if t.(i).(j+1) == 3 then true else
        if t.(i).(j-1) == 3 then true else false

let remove_stones b xs =
  List.iter (fun (i, j) -> b.(i).(j) <- 3) xs

module IntSet = Set.Make(
  struct
    let compare = Pervasives.compare
    type t = int
  end)

(* Find stones of the same color that can be removed. *)
let remove_list b (i, j, init) =
  let lis = ref [] in
  let found_hole = ref false in
  let s = ref IntSet.empty in
  let rec visit (i, j, a) =
    Printf.printf "visit (%d,%d,%d)\n" i j a;
    if a != init then () else
      if out_board b (i, j) then () else
        if !found_hole then () else
          if search_hole b (i, j) then
            found_hole := true
          else
            if IntSet.mem (pos2int (i, j)) !s then () else
            begin
              Printf.printf "For the first time in forever ~~~\n" ;
              lis := (i, j) :: !lis ;

              s := IntSet.add (pos2int (i, j)) !s ;
              IntSet.iter (fun n -> Printf.printf "%d " n) !s ;
              print_newline () ;

              visit (i+1, j, b.(i+1).(j)) ;
              visit (i-1, j, b.(i-1).(j)) ;
              visit (i, j+1, b.(i).(j+1)) ;
              visit (i, j-1, b.(i).(j-1)) ;
            end ;
  in
  visit (i, j, init) ;
  Printf.printf "found hole: %b\nlist: " !found_hole ;
  List.iter (fun (i, j) -> Printf.printf "(%d,%d)" i j) !lis ;
  print_newline () ;
  if !found_hole then [] else !lis 

let remove_list_by_put b (i, j, a) =
  let a' = flip_color a in
  List.fold_left List.append [] [
    remove_list b (i+1, j, a');
    remove_list b (i-1, j, a');
    remove_list b (i, j+1, a');
    remove_list b (i, j-1, a') ]

let can_put t (i, j) = assert false

(* let b = make 19 ;; *)
(* let l = [(4,4); (16,16); (17,14); (14,16); (16,10)] ;; *)
(* put_stones b l ; *)

let remove_test init_list start =
  let b = make 19 in begin
    do_put_stones b init_list ;
    remove_stones b @@ remove_list b start ;
    show b ;
  end
;;

remove_test [(10,10,0);(10,9,1);(10,11,1);(9,10,1);(11,10,1)] (10,10,0) ;
remove_test [(10,10,0);(10,9,1);(10,11,1);(9,10,1);] (10,10,0) ;
remove_test [(9,1,0);(8,1,1);(10,1,1);(9,2,1)] (9,1,0) ;
remove_test [(10,10,0);(11,10,0);(9,10,1);(10,9,1);(10,11,1);(11,9,1);(11,11,1);(12,10,1)] (11,10,0) ;
remove_test [(9,1,0);(8,1,0);(7,1,1);(10,1,1);(8,2,1);(9,2,1)] (9,1,0) ;
