.PHONY: all test install uninstall clean

OB=ocamlbuild -use-ocamlfind
#VERBOSE=-verbose 3
TESTS=001 002 003 004 005 006 007 008 009 010 011 012
#TEST_TARGETS=$(addsuffix .byte, $(addprefix test/test,$(TESTS)) )
INSTALL_TARGETS=pa_ostap.cmo ostap.cm[oix] ostap.o lib/*.mli util/*.mli

.DEFAULT_GOAL: all

all:
	$(OB) lib/ostap.cmo lib/ostap.cmx $(VERBOSE)
	$(OB) -Is lib pa_ostap.cmo $(VERBOSE)
	$(OB) util/util.cmo util/util.cmx
	$(OB) ostap.cmo ostap.cmx



##### Tests stuff
define TESTRULES
BYTE_TEST_EXECUTABLES += test/test$(1).byte
NATIVE_TEST_EXECUTABLES += test/test$(1).native
TEST_MLS += test/test$(1).ml

.PHONY: test_$(1) test$(1).native compile_tests_native compile_tests_byte

test$(1).native: test/test$(1).native
test$(1).byte:   test/test$(1).byte

test/test$(1).byte: test/test$(1).ml
	$(OB) -Is src $$@

test/test$(1).native: test/test$(1).ml
	$(OB) -Is src $$@

run_tests: test_$(1)
test_$(1):
	@cd test  && $(TESTS_ENVIRONMENT) ../test$(1).native; \
	if [ $$$$? -ne 0 ] ; then echo "$(1) FAILED"; else echo "$(1) PASSED"; fi
endef

$(foreach i,$(TESTS),$(eval $(call TESTRULES,$(i)) ) )

.PHONY: compile_tests_native compile_tests_byte compile_tests run_tests


compile_tests_native: $(TEST_MLS)
	$(OB) -Is src $(NATIVE_TEST_EXECUTABLES)

compile_tests_byte: $(TEST_MLS)
	$(OB) -Is src $(BYTE_TEST_EXECUTABLES)

compile_tests: compile_tests_native

clean_tests:
	$(RM) -r _build/test

promote:
	$(MAKE) -C test promote TEST=$(TEST)

tests: lib compile_tests run_tests
test: tests

###### End of tests stuff

install: 
	cd _build && ocamlfind install ostap ../META $(INSTALL_TARGETS)

uninstall:
	ocamlfind remove ostap

clean:
	rm -fr _build test0*.byte

