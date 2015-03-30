.PHONY: default all bin byt clean cleandir configure depend beforedepend

# Compilation rules
.SUFFIXES: .o .c
.SUFFIXES: .cmx .cmxa .cmo .cmi .cma .ml .mli .mll .mly

PACKAGES="lwt,ocamlgraph"

.cmo.o:
	$(PP) "COBJ $@"
	$(CAMLBYT) -custom -output-obj -o $@ $<

.c.o:
	$(CAMLBYT) -ccopt "-fPIC -o $@" -c $<

.ml.cmo:
	$(PP_BYT) $@
	$(OCAMLFIND) ocamlc -package $(PACKAGES) $(CAMLFLAGS) -c $<

.mli.cmi:
	$(PP_BYT) $@
	$(OCAMLFIND) ocamlc  -package $(PACKAGES) -c $<

.ml.cmx:
	$(PP_OPT) $@
	$(OCAMLFIND) ocamlopt  -package $(PACKAGES) $(CAMLFLAGS) -c  $<

.mly.ml:
	$(PP_YACC) $@
	$(CAMLYAC) $(CAMLYACOPTS) $<

.mly.mli:
	$(PP_YACC) $@
	$(CAMLYAC)  $(CAMLYACOPTS) $<

.mll.ml:
	$(PP_LEX) $@
	$(CAMLLEX) $(CAMLLEXOPTS) $<

# Generic clean up
cleandir::
	$(RM) *.cm[ioxa] *.cmxa *.o *.a *.annot *.obj *.lib *~ .*~ a.out .\#*

clean:: cleandir
	($(MAKE) depend) || exit $$?

configure:: cleandir

# Rebuilding dependencies
depend::
	$(CAMLDEP) $(CAMLINCLUDES) $(CAMLFILES) > .depend



include .depend
