FLAGS = -I _build/ -use-menhir -yaccflag --explain

all: clean main.native


main.byte:
	ocamlbuild $(FLAGS) $@
	mv $@ solve

main.native:
	ocamlbuild $(FLAGS) $@
	mv $@ solve


clean:
	ocamlbuild -clean
