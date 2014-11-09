open End_game
open OUnit

let count_test =
  "count_test" >:::
    [ "test1" >:: fun _ ->
      let t = Board.make 5 in
      Board.put_stones t [(3,1);(4,1);(3,2);(4,2);(3,3);(4,3);(3,4);(4,4);(3,5);(4,5)];
      fill t;
      assert_equal 15.0 (count_black t)
    ]

let suite = "end_game_test" >::: [count_test]
