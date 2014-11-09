open OUnit

let suite = 
  "suite" >:::
    [ Board_test.suite;
      Ren_test.suite; 
      End_game_test.suite;
    ]

let _ = run_test_tt_main suite
