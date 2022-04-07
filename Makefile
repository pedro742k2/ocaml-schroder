.PHONY : clean execute

run : program
	./program

program : main.ml
	ocamlfind ocamlopt -o program main.ml -linkpkg -package zarith

mount : main.ml
	ocamlfind ocamlopt -o program main.ml -linkpkg -package zarith

clean:
	rm *.annot *.cmo *.cma *.cmi *.a *.o *.cmx *.cmxs *.cmxa program