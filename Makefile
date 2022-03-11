l: lexer.cmo parser.cmo main.cmo
	ocamlc -o l lexer.cmo parser.cmo main.cmo

main.cmo: lexer.ml parser.ml main.ml
	ocamlc -c main.ml

parser.ml: parser.mly
	ocamlyacc parser.mly

parser.mli: parser.ml

parser.cmi: parser.mli
	ocamlc -c l.mli parser.mli

parser.cmo: parser.cmi
	ocamlc -c parser.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

lexer.mli: lexer.ml

lexer.cmi: lexer.mli
	ocamlc -c lexer.mli

lexer.cmo: lexer.ml parser.cmo
	ocamlc -c lexer.ml

.PHONY: clean
clean:
	rm -f *.cmi *.cmx *.o *.cmo parser.mli parser.ml lexer.mli lexer.ml l
