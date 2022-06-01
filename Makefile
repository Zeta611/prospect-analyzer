l: lexer.cmo parser.cmo main.cmo
	ocamlc -g -o l lexer.cmo parser.cmo main.cmo

main.cmo: lexer.ml parser.ml main.ml
	ocamlc -g -c main.ml

parser.ml: parser.mly
	ocamlyacc parser.mly

parser.mli: parser.ml

parser.cmi: parser.mli
	ocamlc -g -c l.mli parser.mli

parser.cmo: parser.cmi
	ocamlc -g -c parser.ml

lexer.ml: lexer.mll
	ocamllex lexer.mll

lexer.mli: lexer.ml

lexer.cmi: lexer.mli
	ocamlc -g -c lexer.mli

lexer.cmo: lexer.ml parser.cmo
	ocamlc -g -c lexer.ml

.PHONY: clean
clean:
	rm -f *.cmi *.cmx *.o *.cmo parser.mli parser.ml lexer.mli lexer.ml l
