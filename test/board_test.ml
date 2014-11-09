open Core.Std
open OUnit
open Board

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
      "test3" >:: remove_test
      [(9,1,Black);(8,1,Black);(7,1,White);(10,1,White);(8,2,White);(9,1,White)]
      [(7,1,White);(10,1,White);(8,2,White);(9,1,White)]
      ;
      "test4" >:: remove_test
      [(9,1,Black);(8,1,White);(10,1,White);(9,2,White)]
      [(8,1,White);(10,1,White);(9,2,White)]
      ;
      "test5" >:: remove_test
      [(10,10,Black);(11,10,Black);(9,10,White);(10,9,White);(10,11,White);(11,9,White);(11,11,White);(12,10,White)]
      [(9,10,White);(10,9,White);(10,11,White);(11,9,White);(11,11,White);(12,10,White)]
      ;
      "test6" >:: remove_test
      [(2,1,Black);(2,2,White);(2,3,Black);(1,1,White);(1,4,Black);(1,3,White);(1,2,Black)]
      [(2,1,Black);(2,2,White);(2,3,Black);(1,4,Black);(1,2,Black)]
    ]

let kou_test = 
  "kou_test" >::
    fun _ ->
      let t = Board.make 9 in
      put_stones t [(1,2);(2,2);(2,1);(3,1);(3,2);(1,1)];
      assert_equal false (can_put t (2,1,Black));
      put_stones t [(2,3);(3,3)];
      assert_equal true (can_put t (2,1,Black))

let agehama_test =
  "agehama_test" >::
    fun _ ->
      let t = Board.make 9 in
      put_stones t [(1,2);(2,2);(2,1);(3,1);(3,2);(1,1)];
      assert_equal 1 t.agehama.(bw2int White);
      put_stones t [(2,3);(3,3)];
      assert_equal 1 t.agehama.(bw2int Black)

let suite = "board_test" >::: [put_stone_test;kou_test;agehama_test]
