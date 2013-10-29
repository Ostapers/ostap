.PHONY: all test install uninstall clean

#VERBOSE=-verbose 3
TESTS=001 002 003 004 005 006 007 008 009 010 011 012
TEST_TARGETS=$(addsuffix .byte, $(addprefix test/test,$(TESTS)) )
INSTALL_TARGETS=pa_ostap.cmo ostap.cm[oix] ostap.o lib/*.mli util/*.mli

all:
	ocamlbuild -use-ocamlfind lib/ostap.cmo lib/ostap.cmx $(VERBOSE)
	ocamlbuild -use-ocamlfind -Is lib pa_ostap.cmo $(VERBOSE)
	ocamlbuild -use-ocamlfind util/util.cmo util/util.cmx
	ocamlbuild -use-ocamlfind ostap.cmo ostap.cmx

test: 
	ocamlbuild -use-ocamlfind $(TEST_TARGETS) $(VERBOSE)

install: 
	cd _build && ocamlfind install ostap ../META $(INSTALL_TARGETS)

uninstall:
	ocamlfind remove ostap

clean:
	rm -fr _build test0*.byte

