# Basename of the executable we want to produce
EXE=portuml
TOPLEVEL=porttop
JS=portjs
# default target (activate when you invoke only 'make')
default: interpreter

interpreter: $(EXE) $(EXE).byt

repl: $(REPL) $(REPL).byt

all: interpreter repl js

# list of targets that should be recompiled every time
.PHONY: default

CAMLGRAPH = /home/richard/.opam/4.02.1/lib/ocamlgraph
JSOFOCAML = /home/richard/.opam/4.02.1/lib/js_of_ocaml
STUBLIBS = /home/richard/.opam/4.02.1/lib/stublibs/
CAMLINCLUDES = -I $(CAMLGRAPH) -I $(STUBLIBS)
DLLS=-dllpath-pkg lwt,lwt.unix -dllpath $(STUBLIBS)

# Load the default OCaml rules from the file Caml.mk
include ./Caml.mk

PARSER = parser
LEXER = lexer
ANALYSES = analyze_variables
EXEMAIN = exe_main
REPLMAIN = repl_main
JSMAIN = js_main
ENTRIES = $(EXEMAIN) $(REPLMAIN) # $(JSMAIN)
MLFILES_PRE = utils location driver io \
	types values env builtins ast_utils ast
MLFILES_POST = preprocess $(ANALYSES) typer interp
MLOTHER=cfg
MLIFILES =
SRCFILES = $(MLFILES_PRE:%=%.ml) \
	   $(MLFILES_POST:%=%.ml) \
	   $(MLOTHER:%=%.ml) \
	   $(MLIFILES:%=%.mli)

PACKAGES="lwt,lwt.unix"
GENFILES = $(LEXER:%=%.ml) $(PARSER:%=%.ml) $(PARSER:%=%.mli)

CAMLFILES = $(SRCFILES) $(GENFILES) $(ENTRIES:%=%.ml)

JSCMXFILES = $(MLFILES_PRE:%=%.cmx) \
	   $(LEXER:%=%.cmx) \
	   $(PARSER:%=%.cmx) \
	   $(MLFILES_POST:%=%.cmx)

JSCMIFILES = $(MLIFILES:%=%.cmi)
JSCMOFILES = $(JSCMXFILES:%.cmx=%.cmo)

MORECMX = $(MLOTHER:%=%.cmx)
MORECMI = $(MLOTHER:%=%.cmi)
MORECMO = $(MLOTHER:%=%.cmo)

CMXFILES = $(JSCMXFILES) $(MORECMX)
CMIFILES = $(JSCMIFILES) $(MORECMI)
CMOFILES = $(JSCMOFILES) $(MORECMO)

LIBS = graph
OPTLIBDEPS = $(LIBS:%=%.cmxa)
LIBDEPS = $(LIBS:%=%.cma)

JSLIBS = js_of_ocaml.cma


$(EXE): depend $(CMXFILES) $(EXEMAIN).cmx
	$(CAMLFIND) $(CAMLBIN) -package $(PACKAGES) $(CAMLINCLUDES) -linkpkg \
	-o $@ $(OPTLIBDEPS) $(CMXFILES) $(EXEMAIN).cmx


# $(EXE).byt: depend $(CMIFILES) $(CMOFILES) $(EXEMAIN).cmo
#	@echo "Linking bytecode executable ..."
#	$(CAMLBYT) $(CAMLINCLUDES) -o $@ $(LIBDEPS) $(CMOFILES) $(EXEMAIN).cmo

$(EXE).byt: depend $(CMIFILES) $(CMOFILES) $(EXEMAIN).cmo
	@echo "Linking bytecode executable ..."
	$(CAMLFIND) $(CAMLBYT) -package $(PACKAGES) $(CAMLINCLUDES) $(DLLS)  \
	-linkpkg -o $@ $(LIBDEPS) $(CMOFILES) $(EXEMAIN).cmo

$(REPL): depend $(CMXFILES) $(REPLMAIN).cmx
	$(CAMLBIN) $(CAMLINCLUDES) -o $@ $(OPTLIBDEPS) $(CMXFILES) $(REPLMAIN).cmx

$(REPL).byt: depend $(CMIFILES) $(CMOFILES) $(REPLMAIN).cmo
	$(CAMLBYT) $(CAMLINCLUDES) -o $@ $(LIBDEPS) $(CMOFILES) $(REPLMAIN).cmo

JSCAMLBYT = $(CAMLFIND) $(CAMLBYT) -package js_of_ocaml -package js_of_ocaml.syntax \
		-syntax camlp4o -I $(STUBLIBS)


$(JSMAIN).cmo: $(JSMAIN).ml
	$(JSCAMLBYT) -c $<

$(JS).byt: depend $(CMIFILES) $(JSCMOFILES) $(JSMAIN).cmo
	 $(JSCAMLBYT) -linkpkg -o $@ $(JSCMOFILES) $(JSMAIN).cmo

js: $(JS).byt
	$(JSOFOCAML) $<

clean::
	-$(RM) $(EXE) $(EXE).byt *.annot
	for i in $(PARSER) $(LEXER); do\
		$(RM) -f $$i.ml $$i.mli; \
	done
	-$(RM) *.dot *.png *.svg

.PHONY: beforedepend depend codestats


# There are genereated files so we might need to do depend twice
# beforedepend serves exactly that purpose
beforedepend: $(CAMLFILES)
	$(CAMLDEP) $(CAMLINCLUDES) $(CAMLFILES) > .depend

depend:: beforedepend $(CAMLFILES)

OCAMLMETRICS=/home/richard/src/ocaml-metrics/src/ocamlmetrics
codestats:
	@echo "Word count"
	@wc -l $(SRCFILES)
	@$(OCAMLMETRICS) -h $(SRCFILES) > metrics.html
