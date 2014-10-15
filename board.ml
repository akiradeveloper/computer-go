module Board = struct
        type t = int array array
        let make n = Array.make_matrix 0 (n+2) (n+2)
        let show board = assert false
        let put_stone (x, y) t = assert false
end

let b = Board.make 19 ;;
Board.show b
