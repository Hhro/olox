all:
	dune build
	ln -sf _build/default/bin/main.exe olox

test: all
	dune test