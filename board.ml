open Array

type t = {
  matrix: int array array ;
  (* the last kou taken *)
  mutable kou: (int * int) option;
}

let surround (i, j) = [(i+1, j); (i-1, j); (i, j+1); (i, j-1)]
;;

(*
 * black = 0
 * while = 1
 * outside = 2
 * empty = 3
 *)
let make n =
  let b = make_matrix (n+2) (n+2) 2 in
  for i = 1 to n do
    for j = 1 to n do
      b.(i).(j) <- 3
    done
  done ;
  { matrix = b; kou = None }
;;

let flip_color = function
  | 0 -> 1
  | 1 -> 0
  | _ -> assert false
;;

let show t = 
  let () = 
    match t.kou with
    | Some (i, j) -> Printf.printf "kou: (%d, %d)\n" i j
    | None -> Printf.printf "kou: -\n"
  in
  Printf.printf "   [1 2 3 4 5 6 7 8 9 10111213141516171819]\n" ;
  let p i = match i with
  | 0 -> '@'
  | 1 -> 'O'
  | 3 -> ' '
  | _ -> assert false
  in
  for i = 1 to (length t.matrix) - 2 do
    Printf.printf "%2d| " i ;
    for j = 1 to (length t.matrix) - 2 do
      (* Printf.printf "%c " (p (get (get b i) j)) ; *)
      Printf.printf "%c " @@ p t.matrix.(i).(j) ;
    done ;
    print_newline () ;
  done
;;

let out_board t = function
  | (0, _) | (20, _) | (_, 0) | (_, 20) -> true
  | _ -> false
;;

let pos2int (i, j) = (i lsl 5) + j
;;

let int2pos n = (n lsr 5, n land 31)
;;

let search_hole t (i, j) =
  List.exists (fun (i, j) -> t.matrix.(i).(j) = 3) @@ surround (i, j)
;;

module IntSet = Set.Make (
  struct
    let compare = Pervasives.compare
    type t = int
  end )

(* after put. find stones of the same color that can be removed. *)
let remove_list t (i, j, init) =
  let lis = ref [] in
  let found_hole = ref false in
  let s = ref IntSet.empty in
  let rec visit (i, j, a) =
    (* Printf.printf "visit (%d,%d,%d)\n" i j a; *)
    if a != init then () else
      if out_board t.matrix (i, j) then () else
        if !found_hole then () else
          if search_hole t (i, j) then
            found_hole := true
          else
            if IntSet.mem (pos2int (i, j)) !s then () else
            begin
              (* Printf.printf "For the first time in forever ~~~\n" ; *)
              lis := (i, j) :: !lis ;

              s := IntSet.add (pos2int (i, j)) !s ;
              (* IntSet.iter (fun n -> Printf.printf "%d " n) !s ; *)
              print_newline () ;

              List.iter (fun (i, j) -> visit (i, j, t.matrix.(i).(j))) @@ surround (i, j)
            end ;
  in
  visit (i, j, init) ;
  (* Printf.printf "found hole: %b\nlist: " !found_hole ; *)
  (* List.iter (fun (i, j) -> Printf.printf "(%d,%d)" i j) !lis ; *)
  (* print_newline () ; *)
  if !found_hole then [] else !lis
;;

(* after put *)
let remove_list_by_put t (i, j, a) =
  let a' = flip_color a in
  List.fold_left List.append [] @@
  List.map (fun (i, j) -> remove_list t (i, j, a')) @@ surround (i, j)
;;

(* before put *)
let will_take_one t (i, j, a) =
  let r = ref false in
  t.matrix.(i).(j) <- a ;
  r := List.length @@ remove_list_by_put t (i, j, a) = 1;
  t.matrix.(i).(j) <- 3 ;
  !r
;;

(* before put *)
let try_suicide t (i, j, a) =
  let r = ref [] in
  t.matrix.(i).(j) <- a ;
  r := remove_list t (i, j, a) ;
  t.matrix.(i).(j) <- 3 ;
  !r
;;

(* before put *)
let is_single_suicide t (i, j, a) =
  match try_suicide t (i, j, a) with
  | [_] -> true
  | _ -> false
;;

(* before put *)
let is_suicide t (i, j, a) =
  try_suicide t (i, j, a) = []
;;

(* before put *)
let is_kou_take t (i, j, a) =
  is_single_suicide t (i, j, a)  && 
  will_take_one t (i, j, a)
;;

(* before put *)
let can_put t (i, j, a): bool =
  let stone_exists = t.matrix.(i).(j) < 2 in
  let koudate_need = match t.kou with
  | Some (i', j') -> (i, j) = (i', j')
  | _ -> false
  in
  not @@ (stone_exists || koudate_need || is_suicide t (i, j, a))
;;

let remove_stones t xs =
  List.iter (fun (i, j) -> t.matrix.(i).(j) <- 3) xs
;;

let put_stone t (i, j, a) =
  show t ;
  let was_kou_take = is_kou_take t (i, j, a) in
  t.matrix.(i).(j) <- a ;
  let xs = remove_list_by_put t (i, j, a) in
  remove_stones t xs ;
  (* if this put is kou-take then we remember the point *)
  if was_kou_take then
    t.kou <- Some (List.hd xs)
  else 
    t.kou <- None
;;

(* after pass, it is allowed to put on kou point *)
let pass t =
  t.kou <- None

let do_put_stones t xs =
  List.iter (fun (i, j, a) -> put_stone t (i, j, a)) xs
;;

let put_stones t xs =
  let rec zip xs ys =
    match (xs, ys) with
    | ([], _) -> []
    | (_, []) -> []
    | ((i,j) :: xs', a :: ys') -> (i, j, a) :: (zip xs' ys') in
  let alt_color xs = zip xs (List.mapi (fun i _ -> i mod 2) xs) in
  do_put_stones t @@ alt_color xs
;;

let t = make 19 ;;
(* let l = [(4,4); (16,16); (17,14); (14,16); (16,10)] ;; *)
(* put_stones b l ; *)

let remove_test init_list start =
  let t = make 19 in
  do_put_stones t init_list ;
  remove_stones t @@ remove_list t start ;
  show t ;
;;

(* remove_test [(10,10,0);(10,9,1);(10,11,1);(9,10,1);(11,10,1)] (10,10,0) ; *)
(* remove_test [(10,10,0);(10,9,1);(10,11,1);(9,10,1);] (10,10,0) ; *)
(* remove_test [(9,1,0);(8,1,1);(10,1,1);(9,2,1)] (9,1,0) ; *)
(* remove_test [(10,10,0);(11,10,0);(9,10,1);(10,9,1);(10,11,1);(11,9,1);(11,11,1);(12,10,1)] (11,10,0) ; *)
(* remove_test [(9,1,0);(8,1,0);(7,1,1);(10,1,1);(8,2,1);(9,2,1)] (9,1,0) ; *)

(* expect double kill *)
(* put_stones t [(2,1);(2,2);(2,3);(1,1);(1,4);(1,3);(1,2)] ; *)
put_stones t [(1,2);(2,2);(2,1);(3,1);(3,2)] ;
put_stone t (1,1,1) ;
assert (not @@ can_put t (2,1,0)) ;
put_stone t (2,3,0) ;
put_stone t (3,3,1) ;
assert (can_put t (2,1,0)) ;
put_stone t (2,1,0) ;
show t ;
