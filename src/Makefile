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

CAMLGRAPH = /home/richard/devel/ocamlgraph
JSOFOCAML = /home/richard/.opam/4.01.0/lib/js_of_ocaml

CAMLINCLUDES = -I $(CAMLGRAPH) -I $(JSOFOCAML)


# Load the default OCaml rules from the file Caml.mk
include ./Caml.mk

PARSER = parser
LEXER = lexer
ANALYSES = analyze_variables
EXEMAIN = exe_main
REPLMAIN = repl_main
JSMAIN = js_main
ENTRIES = $(EXEMAIN) $(REPLMAIN) # $(JSMAIN)
MLFILES_PRE = utils location base driver io builtins ast  ast_utils
MLFILES_POST = $(ANALYSES) typer interp
MLOTHER=cfg
MLIFILES =
SRCFILES = $(MLFILES_PRE:%=%.ml) \
	   $(MLFILES_POST:%=%.ml) \
	   $(MLOTHER:%=%.ml) \
	   $(MLIFILES:%=%.mli)

GENFILES = $(LEXER:%=%.ml) $(PARSER:%=%.ml)

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

LIBS = str graph
OPTLIBDEPS = $(LIBS:%=%.cmxa)
LIBDEPS = $(LIBS:%=%.cma)

JSLIBS = js_of_ocaml.cma str.cma
STUBLIBS = /home/richard/.opam/4.01.0/lib/stublibs/

$(EXE): depend $(CMXFILES) $(EXEMAIN).cmx
	$(CAMLBIN) $(CAMLINCLUDES) -o $@ $(OPTLIBDEPS) $(CMXFILES) $(EXEMAIN).cmx

$(EXE).byt: depend $(CMIFILES) $(CMOFILES) $(EXEMAIN).cmo
	@echo "Linking bytecode executable ..."
	$(CAMLBYT) $(CAMLINCLUDES) -o $@ $(LIBDEPS) $(CMOFILES) $(EXEMAIN).cmo

$(REPL): depend $(CMXFILES) $(REPLMAIN).cmx
	$(CAMLBIN) $(CAMLINCLUDES) -o $@ $(OPTLIBDEPS) $(CMXFILES) $(REPLMAIN).cmx

$(REPL).byt: depend $(CMIFILES) $(CMOFILES) $(REPLMAIN).cmo
	$(CAMLBYT) $(CAMLINCLUDES) -o $@ $(LIBDEPS) $(CMOFILES) $(REPLMAIN).cmo

CAMLFIND = /home/richard/.opam/4.01.0/bin/ocamlfind
JSCAMLBYT = $(CAMLFIND) $(CAMLBYT) -package js_of_ocaml -package js_of_ocaml.syntax \
		-syntax camlp4o -I $(STUBLIBS)
JSOFOCAMLEXE = /home/richard/.opam/4.01.0/bin/js_of_ocaml


$(JSMAIN).cmo: $(JSMAIN).ml
	$(JSCAMLBYT) -c $<

$(JS).byt: depend $(CMIFILES) $(JSCMOFILES) $(JSMAIN).cmo
	 $(JSCAMLBYT) -linkpkg -o $@ $(JSLIBS) $(JSCMOFILES) $(JSMAIN).cmo

js: $(JS).byt
	$(JSOFOCAMLEXE) $<

clean::
	-$(RM) $(EXE) $(EXE).byt *.annot
	for i in $(PARSER) $(LEXER); do\
		$(RM) -f $$i.ml $$i.mli; \
	done

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
