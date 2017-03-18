objs = parser.cmo scanner.cmo

all: scanner.ml parser.ml parser.mli ast.cmi parser.cmi scanner.cmo parser.cmo

parser.ml parser.mli:
	ocamlyacc parser.mly

%.cmi: %.ml
	ocamlc -c $<

%.cmo: %.ml
	ocamlc -c $<

scanner.ml: scanner.mll
	ocamllex $<

clean:
	rm -f calc parser.ml parser.mli scanner.ml *.cmo *.cmi
