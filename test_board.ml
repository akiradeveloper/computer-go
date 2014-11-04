open Board

let t = Board.make 19
(* let l = [(4,4); (16,16); (17,14); (14,16); (16,10)] ;; *)
(* put_stones b l ; *)

let remove_test init_list start =
  let t = make 19 in
  do_put_stones t init_list;
  remove_stones t @@ remove_list t start;
  show t;

(* remove_test [(10,10,0);(10,9,1);(10,11,1);(9,10,1);(11,10,1)] (10,10,0) ; *)
(* remove_test [(10,10,0);(10,9,1);(10,11,1);(9,10,1);] (10,10,0) ; *)
(* remove_test [(9,1,0);(8,1,1);(10,1,1);(9,2,1)] (9,1,0) ; *)
(* remove_test [(10,10,0);(11,10,0);(9,10,1);(10,9,1);(10,11,1);(11,9,1);(11,11,1);(12,10,1)] (11,10,0) ; *)
(* remove_test [(9,1,0);(8,1,0);(7,1,1);(10,1,1);(8,2,1);(9,2,1)] (9,1,0) ; *)

(* expect double kill *)
(* put_stones t [(2,1);(2,2);(2,3);(1,1);(1,4);(1,3);(1,2)] ; *)
put_stones t [(1,2);(2,2);(2,1);(3,1);(3,2)];
put_stone t (1,1,1);
assert (not @@ can_put t (2,1,0));
put_stone t (2,3,0);
put_stone t (3,3,1);
assert (can_put t (2,1,0));
put_stone t (2,1,0);
show t;
