all: build

clean:
	rm -rf bin

build:
	mkdir -p bin
	cd bin && scalac ../src/Chess.scala ../src/Main.scala

run:
	@echo "Running Scala Chess..."
	cd bin && scala -cp . --main-class Main