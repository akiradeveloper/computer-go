open OUnit
open Array
open Board
open Ren

let t = Board.make 19
;;
put_stones t [(1,2);(2,2);(2,1);(3,1);(3,2)];
put_stone t (1,1,1);
assert (not @@ can_put t (2,1,0));
put_stone t (2,3,0);
put_stone t (3,3,1);
assert (can_put t (2,1,0));
put_stone t (2,1,0);
put_stone t (4,1,1);
Ren.show @@ Ren.make t.matrix;

let delete_color groups = List.map ~f:(List.map ~f:fun (i,j,a) -> (i,j))

let compress groups =
  List.map ~f:(List.map ~f:Board.pos2int) groups |>
  List.fold_left ~init:0 ~f:(+)

let group_test' puts groups =
  fun _ ->
    let t = Board.make 19 in
    put_stones t puts;
    let ren = Ren.make t.matrix in
    assert_equal (compress groups) (Ren.list_groups ren |> delete_color |> compress)

let group_test = 
  "group_test" >:::
    [ "test1" >:: group_test'
      [(1,2);(2,2);(2,1);(3,1);(3,2);(1,1);(2,3);(3,3);(2,1);(4,1)]
      [[(1,2)];[(2,1)];[(2,3)];[(3,1);(4,1)];[(3,2)];[(3,3)]]
    ]

let suite = "ren_test" >::: [group_test]
