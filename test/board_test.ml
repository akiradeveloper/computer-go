open OUnit
open Board

let t = Board.make 19

let remove_test init_list last =
  let t = make 19 in
  do_put_stones t init_list;
  show t;
  put_stone t last;
  show t;
;;

remove_test [(10,10,0);(10,9,1);(10,11,1);(9,10,1)] (11,10,1);
remove_test [(10,9,1);(10,11,1);(9,10,1)] (10,10,0);
remove_test [(8,1,1);(10,1,1);(9,2,1)] (9,1,0);
remove_test [(10,10,0);(11,10,0);(9,10,1);(10,9,1);(10,11,1);(11,9,1);(11,11,1)] (12,10,1);
remove_test [(9,1,0);(8,1,0);(7,1,1);(10,1,1);(8,2,1)] (9,2,1);
remove_test [(2,1,0);(2,2,1);(2,3,0);(1,1,1);(1,4,0);(1,3,1);] (1,2,0);
