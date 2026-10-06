all: build

clean:
	rm -rf bin

build:
	mkdir -p bin
	cd bin && scalac ../src/TicTacToe.scala ../src/Main.scala

run:
	@echo "Running Scala version..."
	cd bin && scala run -cp . --main-class Main

run_ocaml:
	@echo "Running OCaml version..."
	ocaml tictactoe.ml