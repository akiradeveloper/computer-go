open Board
open End_game
open OUnit

let small_board_test =
  "test" >::
    fun _ ->
      let t = Board.make 5 in
      Board.show t;
      assert_equal 0 0

let count_test =
  "count_test" >:::
    [ "test1" >:: (fun _ ->
        let t = Board.make 5 in
        Board.put_stones t [(3,1);(4,1);(3,2);(4,2);(3,3);(4,3);(3,4);(4,4);(3,5);(4,5)];
        Board.show t;
        fill t;
        Board.show t;
        assert_equal 15. (count_black t))
      ;
      "test2" >:: (fun _ ->
        let t = Board.make 9 in
        Board.do_put_stones t
        [(7,3,White);(7,4,White);(7,5,White);(7,6,White);(7,7,White);(7,8,White);(7,9,White);(8,3,White);(9,3,White);(9,6,White);(9,7,White);(9,8,White);
        (8,4,Black);(8,5,Black);(8,6,Black);(8,7,Black);(8,8,Black);(8,9,Black);(9,4,Black);
        (5,1,Black);(5,2,Black);(5,3,Black);(5,4,Black);(5,5,Black);(5,6,Black);(5,7,Black);(5,8,Black);(5,9,Black)];
        Board.show t;
        fill t;
        show t;
        assert_equal 53. (count_black t))
    ]

let suite = "end_game_test" >::: [
  (* small_board_test; *)
  count_test
]
