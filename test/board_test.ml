open Core.Std
open OUnit
open Board

(* remove_test [(10,10,0);(10,9,1);(10,11,1);(9,10,1)] (11,10,1); *)
(* remove_test [(10,9,1);(10,11,1);(9,10,1)] (10,10,0); *)
(* remove_test [(8,1,1);(10,1,1);(9,2,1)] (9,1,0); *)
(* remove_test [(10,10,0);(11,10,0);(9,10,1);(10,9,1);(10,11,1);(11,9,1);(11,11,1)] (12,10,1); *)
(* remove_test [(9,1,0);(8,1,0);(7,1,1);(10,1,1);(8,2,1)] (9,2,1); *)
(* remove_test [(2,1,0);(2,2,1);(2,3,0);(1,1,1);(1,4,0);(1,3,1);] (1,2,0); *)

let print_stones xs =
  List.iter ~f:(fun (i,j,a) -> Printf.printf "(%d,%d,%d)" i j a) xs;
  Printf.printf "\n"

let set_eq xs ys =
  let sor xs' =
    List.sort ~cmp:(fun (i1,j1,_) (i2,j2,_) -> compare (pos2int (i1,j1)) (pos2int (i2,j2))) xs'
  in
  (* print_stones (sor ys); *)
  (* print_stones (sor xs); *)
  assert_equal (sor ys) (sor xs)

let remove_test xs result =
  fun _ ->
    let t = Board.make 19 in
    do_put_stones t xs;
    set_eq (list_stones t) result

let put_stone_test =
  "put_stone_test" >:::
    [
      "test1" >:: remove_test
      [(10,10,Black);(10,9,White);(10,11,White);(9,10,White);(11,10,White)]
      [(10,9,White);(10,11,White);(9,10,White);(11,10,White)]
      ;
      "test2" >:: remove_test
      [(10,9,White);(10,11,White);(9,10,White);(10,10,Black)]
      [(10,9,White);(10,11,White);(9,10,White);(10,10,Black)]
      ;
    ]

let kou_test = 
  "kou_test" >::
    fun _ ->
      let t = Board.make 19 in
      put_stones t [(1,2);(2,2);(2,1);(3,1);(3,2);(1,1)];
      assert_equal false (can_put t (2,1,Black));
      put_stones t [(2,3);(3,3)];
      assert_equal true (can_put t (2,1,Black))
;;

let suite = "board_test" >::: [put_stone_test;kou_test]
