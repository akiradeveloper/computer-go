open Array
open Board

let t = Board.make 19
put_stones t [(1,2);(2,2);(2,1);(3,1);(3,2)];
put_stone t (1,1,1);
assert (not @@ can_put t (2,1,0));
put_stone t (2,3,0);
put_stone t (3,3,1);
assert (can_put t (2,1,0));
put_stone t (2,1,0);
put_stone t (4,1,1);
Ren.show @@ Ren.make t.matrix;
