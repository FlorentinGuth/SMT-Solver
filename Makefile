FLAGS = -I _build/ -use-menhir

all: main.byte


main.byte:
	ocamlbuild $(FLAGS) $@
	mv $@ solve


clean:
	ocamlbuild -clean
