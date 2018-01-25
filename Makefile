all:
	ocamlbuild -pkgs llvm,llvm.analysis main.byte

clean:
	ocamlbuild -clean

