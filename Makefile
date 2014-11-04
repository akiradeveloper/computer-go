board: test_board.ml src/board.ml
	ocamlbuild test_board.native -I src
	./test_board.native

ren: test_ren.ml src/ren.ml
	ocamlbuild test_ren.native -I src
	./test_ren.native
