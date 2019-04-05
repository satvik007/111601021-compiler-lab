SHELL := /bin/bash

all: Cal_ast.sml Cal.grm Cal.lex CalCompile.sml translate.sml source.mlb
	mllex Cal.lex
	mlyacc Cal.grm
	mlton source.mlb

clean:
	rm  -rf *.grm.desc *.grm.sig *.grm.sml *.lex.sml source

test: source
		bash test.sh
