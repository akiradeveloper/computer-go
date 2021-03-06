open Core.Std
open Board
open Array

type t = {
  matrix: int array array;
}

type attr = {
  color: int;
}

let show t = 
  Printf.printf "   [1 2 3 4 5 6 7 8 9 10111213141516171819]\n";
  for i = 1 to (length t) - 2 do
    Printf.printf "%2d| " i;
    for j = 1 to (length t) - 2 do
      Printf.printf "%2d" @@ t.(i).(j);
    done ;
    print_newline ();
  done

let make mat =
  let n = length mat in 
  let result = make_matrix n n 0 in
  let number = ref 1 in
  let rec f (i, j, a) id =
    if result.(i).(j) > 0 then
      false
    else
      (* Outside is eliminated by this check *)
      if mat.(i).(j) = a then
        begin
          result.(i).(j) <- id;
          ignore @@ f (i+1, j, a) id;
          ignore @@ f (i-1, j, a) id;
          ignore @@ f (i, j+1, a) id;
          ignore @@ f (i, j-1, a) id;
          true
        end
      else
        false
  in
  let g = 
    for i = 1 to (n - 2) do
      for j = 1 to (n - 2) do
        let a = mat.(i).(j) in
        if stone_exists' a then
          (* Not empty *)
          if f (i, j, a) !number then
            number := !number + 1;
      done
    done 
  in
  g;
  result

let list_groups matrix =
  let n = Array.length matrix in
  let r = Array.create ((n-2)*(n-2)) [] in
  for i = 1 to n-2 do
    for j = 1 to n-2 do
      let g = matrix.(i).(j) in
      if g <> 0 then
        r.(g) <- (i,j) :: r.(g)
    done
  done;
  Array.to_list r |> List.filter ~f:fun a -> (a <> [])
