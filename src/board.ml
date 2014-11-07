open Core.Std
open Array

type t = {
  matrix: int array array;
  mutable kou: (int * int) option; (* the last kou put *)
  mutable agehama: int array;
}

let size t = (Array.length t.matrix) - 2

let surround (i, j) = [(i+1, j); (i-1, j); (i, j+1); (i, j-1)]

(*
 * black = 0
 * while = 1
 * outside = 2
 * empty = 3
 *)
let make n =
  let b = make_matrix ~dimx:(n+2) ~dimy:(n+2) 2 in
  for i = 1 to n do
    for j = 1 to n do
      b.(i).(j) <- 3
    done
  done ;
  { matrix = b; kou = None; agehama = Array.create 2 0; }

let list_stones t =
  let lis = ref [] in
  for i = 1 to size t do
    for j = 1 to size t do
      let e = t.matrix.(i).(j) in
      if e <> 3 then
        lis := (i, j, e) :: !lis
    done
  done;
  !lis

let flip_color = function
  | 0 -> 1
  | 1 -> 0
  | _ -> assert false

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
  | 3 -> '+'
  | _ -> assert false
  in
  for i = 1 to (length t.matrix) - 2 do
    Printf.printf "%2d| " i;
    for j = 1 to (length t.matrix) - 2 do
      Printf.printf "%c " @@ p t.matrix.(i).(j);
    done;
    print_newline ();
  done

let out_board t (i, j) =
  if i < 1 || i > size t || j < 1 || j > size t then true
  else false

let pos2int (i, j) = (i lsl 5) + j

let int2pos n = (n lsr 5, n land 31)

let search_hole t (i, j) =
  List.exists ~f:(fun (i, j) -> t.matrix.(i).(j) = 3) @@ surround (i, j)

module IntSet = Set.Make (
  struct
    type t = int with sexp, compare
  end )

(* after put. find stones of the same color that can be removed. *)
let remove_list t (i, j, init) =
  let lis = ref [] in
  let found_hole = ref false in
  let s = ref IntSet.empty in
  let rec visit (i, j, a) =
    (* Printf.printf "visit (%d,%d,%d)\n" i j a; *)
    if a <> init then () else
      if out_board t (i, j) then () else
        if !found_hole then () else
          if search_hole t (i, j) then
            found_hole := true
          else
            if IntSet.mem !s (pos2int (i, j)) then () else
            begin
              (* Printf.printf "For the first time in forever ~~~\n" ; *)
              lis := (i, j) :: !lis;

              s := IntSet.add !s (pos2int (i, j));
              (* IntSet.iter (fun n -> Printf.printf "%d " n) !s ; *)
              print_newline ();

              List.iter ~f:(fun (i, j) -> visit (i, j, t.matrix.(i).(j))) @@ surround (i, j)
            end
  in
  visit (i, j, init);
  (* Printf.printf "found hole: %b\nlist: " !found_hole ; *)
  (* List.iter (fun (i, j) -> Printf.printf "(%d,%d)" i j) !lis ; *)
  (* print_newline () ; *)
  if !found_hole then [] else !lis

(* after put *)
let remove_list_by_put t (i, j, a) =
  let a' = flip_color a in
  List.fold_left ~f:List.append ~init:[] @@
  List.map ~f:(fun (i, j) -> remove_list t (i, j, a')) @@ surround (i, j)

(* before put *)
let try_remove_list t (i, j, a) =
  let r = ref [] in
  t.matrix.(i).(j) <- a;
  r := remove_list_by_put t (i, j, a);
  t.matrix.(i).(j) <- 3;
  !r

(* before put *)
let will_take_one t (i, j, a) =
  List.length (try_remove_list t (i, j, a)) = 1

(* before put *)
let try_suicide t (i, j, a) =
  let r = ref [] in
  t.matrix.(i).(j) <- a;
  r := remove_list t (i, j, a);
  t.matrix.(i).(j) <- 3;
  !r

(* before put *)
let is_single_suicide t (i, j, a) =
  match try_suicide t (i, j, a) with
  | [_] -> true
  | _ -> false

(* before put *)
let is_suicide t (i, j, a) =
  try_suicide t (i, j, a) = []

(* before put *)
let is_kou_take t (i, j, a) =
  is_single_suicide t (i, j, a)  && 
  will_take_one t (i, j, a)
;;

(* before put *)
let can_put t (i, j, a) =
  let stone_exists = t.matrix.(i).(j) < 2 in
  let koudate_need = match t.kou with
  | Some (i', j') -> (i, j) = (i', j')
  | _ -> false
  in
  not @@ (stone_exists || koudate_need || is_suicide t (i, j, a))

let list_can_put t a = 
  let r = ref [] in
  let sz = (Array.length t.matrix) - 2 in
  for i = 1 to sz do
    for j = 1 to sz do
      if can_put t (i, j, a) then
        r := (i, j) :: !r
    done
  done;
  !r

let remove_stones t xs =
  List.iter ~f:(fun (i, j) -> t.matrix.(i).(j) <- 3) xs

let put_stone t (i, j, a) =
  (* show t; *)
  let was_kou_take = is_kou_take t (i, j, a) in
  t.matrix.(i).(j) <- a;
  let xs = remove_list_by_put t (i, j, a) in
  remove_stones t xs;
  (* if this put is kou-take then we remember the point *)
  if was_kou_take then
    t.kou <- Some (List.hd_exn xs)
  else 
    t.kou <- None

(* after pass, it is allowed to put on kou point *)
let pass t =
  t.kou <- None

let do_put_stones t xs =
  List.iter ~f:(fun (i, j, a) -> put_stone t (i, j, a)) xs

let put_stones t xs =
  let rec zip xs ys =
    match (xs, ys) with
    | ([], _) -> []
    | (_, []) -> []
    | ((i,j) :: xs', a :: ys') -> (i, j, a) :: (zip xs' ys') in
  let alt_color xs = zip xs (List.mapi ~f:(fun i _ -> i mod 2) xs) in
  do_put_stones t @@ alt_color xs
