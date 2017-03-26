ocamlc -c tokenizer.ml
ocamlc -o tokenizer parser.cmo scanner.cmo tokenizer.cmo
./tokenizer < $1 | menhir --interpret --interpret-show-cst parser.mly