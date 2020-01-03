.PHONY: run test vif vit clean install ocaml

rungosh:
	pac -f -v gauche  t.pac  
	@echo -----
	@cat t.scm

run: rungosh
	@#perl ./pac.pl -p kawa -l scheme t.pac | tee t.scm 

runkawa:
	@#perl ./pac.pl -p kawa -d scheme t.pac | tee t.scm 
	@pac kawa  t.pac  
	@kawa -Dkawa.import.path=".:libs/kawa/*.scm" t.scm

runocaml: #ocaml
	@perl ./pac.pl -p ocaml -l scheme t.pac | tee t.scm 
	rm -f libs/utils.cmi
	ocamlc -c libs/utils.ml
	scm2ml t.scm | tee t.ml 
	ocamlc str.cma -I libs utils.cmo t.ml  
	./a.out
	@#perl ./pac.pl -p gauche -l scheme t.pac | tee t.scm 
	@#gosh -I libs/gauche t.scm

test: clean
	@sh ./test.sh scheme tt
	@#sh ./test.sh scheme 
	@#sh ./test.sh clojure 

libs/utils.cmo:
	rm -f libs/utils.cmi
	ocamlc -c libs/utils.ml

ocaml: libs/utils.cmo
	perl ./pac.pl -p ocaml -l scheme t.pac | tee t.scm && scm2ml t.scm | tee t.ml && ocamlc -I libs utils.cmo t.ml  && ./a.out

vit: test

vif:  run

clean:
	rm -f t/*.out
	rm -f *.cm* *.scm
	rm -f libs/*.cm*
	rm -f out a.out


install:
	mkdir -p ~/tools/bin
	@perl -pe "s:\@\@CWD\@\@:$(PWD):g" pac.in > ~/tools/bin/pac 
	chmod 0755 ~/tools/bin/pac

