open Board
open Core.Std

let rec inc_list a n =   
  List.init n ~f:fun i -> a+i 

let rec list_make a n =
  List.init n ~f:fun i -> a

let to_color t xs =
  List.map xs ~f:fun (i,j) ->
    (t.matrix.(i).(j))

let ray_up t (i, j) =
  let n = i in
  List.zip_exn (inc_list 0 n) (list_make j n) |>
  to_color t |>
  List.rev 

let ray_down t (i, j) =
  let n = (size t) - i - 1 in
  List.zip_exn (inc_list (i+1) n) (list_make j n) |>
  to_color t

let ray_left t (i, j) = 
  let n = j in
  List.zip_exn (inc_list 0 n) (list_make i n) |>
  to_color t

let ray_right t (i, j) =
  let n = (size t) - j - 1 in
  List.zip_exn (inc_list (j+1) n) (list_make i n) |>
  to_color t

let list_ray_hit t (i, j) =
  let p = List.find_exn ~f:(fun a -> a <> 2)
  in
  [
    p (ray_up t (i,j));
    p (ray_down t (i,j));
    p (ray_left t (i,j));
    p (ray_right t (i,j));
  ]

let is_dame' xs =
  match (List.find ~f:(fun a -> a = 0) xs, List.find ~f:(fun a -> a = 1) xs) with
  | (Some _, Some _) -> true
  | _ -> false

(* In chinese rule. dame is possessed by both players and
 * the definition is a point surrounded by living stones *)
let is_dame t (i, j) =
  list_ray_hit t (i, j) |> is_dame'

let list_dame t =
  list_locs t |> List.filter ~f:(is_dame t)

let finish mat = assert false

let fill_dame t = assert false

let fill t = assert false

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
