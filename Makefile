FLAGS = -I _build/ -use-menhir -yaccflag --explain

all: clean main.native test_generator.native


main.byte:
	ocamlbuild $(FLAGS) $@
	mv $@ solve

main.native:
	ocamlbuild $(FLAGS) $@
	mv $@ solve

test_generator.native:
	ocamlbuild $(FLAGS) $@
	mv $@ generate


clean:
	ocamlbuild -clean
