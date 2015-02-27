######################################################################
#                                                                    #
#                           Moca                                     #
#                                                                    #
#          Pierre Weis, INRIA Rocquencourt                           #
#          Frédéric Blanqui, projet Protheo, INRIA Lorraine          #
#                                                                    #
#  Copyright 2005-2012,                                              #
#  Institut National de Recherche en Informatique et en Automatique. #
#  All rights reserved.                                              #
#                                                                    #
#  This file is distributed under the terms of the Q Public License. #
#                                                                    #
######################################################################

# $Id: Caml.mk,v 1.32 2012-06-04 13:01:21 weis Exp $

# This Makefile defines generic rules to compile various kinds of Caml source
# files and to automatically handle their dependencies.

# This Makefile also defines two entries:
# - clean (to clean up the compiled files)
# - depend (to recompute the dependency order).

# This Makefile should be included at the end of the Makefile that handles a
# set of Caml files (to build a library or an application).
# Simpy write at the end of your Makefile:
# include path_to_Caml.mk/Caml.mk
CAMLBYT=ocamlc
CAMLBIN=ocamlopt
CAMLLEX=ocamllex
CAMLYAC=menhir
RM=rm -f
CAMLDEP=ocamldep
CAMLFIND=ocamlfind
CAMLFLAGS=-w +a-4 -warn-error A -g -annot

.PHONY: default all bin byt clean cleandir configure depend beforedepend

# Compilation rules
.SUFFIXES:
.SUFFIXES: .cmx .cmxa .cmo .cmi .cma .ml .mli .mlin .mliin .mlm .mlms .mll .mly
.SUFFIXES: .htm .html .shtml .data .1 .man

.ml.cmo:
	@echo "Byte Compiling $<"
	@$(CAMLFIND) $(CAMLBYT) -package $(PACKAGES) $(CAMLINCLUDES) $(CAMLFLAGS) -c $<

.mli.cmi:
	@echo "Compiling $<"
	@$(CAMLFIND) $(CAMLBYT) -package $(PACKAGES) $(CAMLINCLUDES) -c $<

.ml.cmx:
	@echo "Native Compiling $<"
	@$(CAMLFIND) $(CAMLBIN) -package $(PACKAGES) $(CAMLINCLUDES) $(CAMLFLAGS) -c  $<

.mly.ml:
	$(CAMLYAC) $<

.mly.mli:
	$(CAMLYAC) $<

.mll.ml:
	$(CAMLLEX) $<


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
