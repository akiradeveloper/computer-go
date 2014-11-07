open Core.Std
open OUnit
open Board
open Ren

let compress groups =
  List.map ~f:(List.map ~f:Board.pos2int) groups |>
  List.map ~f:(List.fold_left ~init:0 ~f:(+)) |>
  List.fold_left ~init:0 ~f:(+)

let group_test' puts groups =
  fun _ ->
    let t = Board.make 19 in
    put_stones t puts;
    let ren = Ren.make t.matrix in
    assert_equal (compress groups) (Ren.list_groups ren |> compress)

let group_test = 
  "group_test" >:::
    [ "test1" >:: group_test'
      [(1,2);(2,2);(2,1);(3,1);(3,2);(1,1);(2,3);(3,3);(2,1);(4,1)]
      [[(1,2)];[(2,1)];[(2,3)];[(3,1);(4,1)];[(3,2)];[(3,3)]]
    ]

let suite = "ren_test" >::: [group_test]
