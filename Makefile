board: test/board_test.ml src/board.ml
	ocamlbuild test/board_test.native -I src
	./board_test.native

ren: test/ren_test.ml src/ren.ml
	ocamlbuild test/ren_test.native -I src
	./ren_test.native
