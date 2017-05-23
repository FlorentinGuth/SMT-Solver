FLAGS = -I _build/ -use-menhir -yaccflag --explain

all: main.byte


main.byte:
	ocamlbuild $(FLAGS) $@
	mv $@ solve


clean:
	ocamlbuild -clean
