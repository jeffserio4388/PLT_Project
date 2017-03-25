OBJS = ast.cmo parser.cmo scanner.cmo semant.cmo codegen.cmo pipeline.cmo

pipeline : $(OBJS)
	ocamlc -o pipeline $(OBJS)

parser.ml parser.mli:
	ocamlyacc parser.mly

%.cmo: %.ml
	ocamlc -c $<

%.cmi: %.mli
	ocamlc -c $<

ast.cmi:
	ocamlc -c ast.ml

scanner.ml: scanner.mll
	ocamllex $<

clean:
	rm -f calc parser.ml parser.mli scanner.ml *.cmo *.cmi

ast.cmo:
ast.cmx:
semant.cmo: ast.cmo
semant.cmx: ast.cmx
codegen.cmo: ast.cmo
codegen.cmx: ast.cmx
pipeline.cmo: semant.cmo scanner.cmo parser.cmi codegen.cmo ast.cmo
	ocamlc -custom unix.cma parser.ml $< -cclib -linux
pipeline.cmx: semant.cmx scannser.cmx parser.cmx codegen.cmx ast.cmx
parser.cmo: parser.cmi
parser.cmx: ast.cmx
scanner.cmo : ast.cmo
scanner.cmx: parser.cmx
parser.cmi: ast.cmo
