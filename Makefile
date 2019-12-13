.PHONY: run test vif vit clean install ocaml


run: #ocaml
	@perl ./pac.pl -l scheme m.ms | tee m.scm 
	@echo ----
	@sagittarius m.scm
	@#t/310-let-nest-doublepoint.t

test: clean
	@sh ./test.sh scheme 
	@sh ./test.sh clojure 

libs/utils.cmo:
	rm -f libs/utils.cmi
	ocamlc -c libs/utils.ml

ocaml: libs/utils.cmo
	perl ./pac.pl -l clojure t.ms | tee t.scm && scm2ml t.scm | tee t.ml && ocamlc -I libs utils.cmo t.ml  && ./a.out

vit: test

vif:  run

clean:
	rm -f t/*.out
	rm -f *.cm* *.scm
	rm -f libs/*.cm*
	rm -f out a.out


install:
	@mkdir -p ~/tools/bin
	@rm -f ~/tools/bin/pac
	@ln -s $(shell pwd)/pac.pl ~/tools/bin/pac
	@chmod 0755 ~/tools/bin/pac

