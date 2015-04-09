# Basename of the executable we want to produce
EXE=portuml
TOPLEVEL=porttop
JS=portjs
# default target (activate when you invoke only 'make')
default: interpreter js

interpreter: $(EXE) $(EXE).byt

repl: $(REPL) $(REPL).byt

all: interpreter repl js

# list of targets that should be recompiled every time
.PHONY: default

include ../Config.mk

# Load the default OCaml rules from the file Caml.mk
include ./Caml.mk

PARSER = parser
LEXER = lexer
ANALYSES = analyze_variables
EXEMAIN = exe_main
REPLMAIN = repl_main
JSMAIN = js_main
ENTRIES = $(EXEMAIN) $(REPLMAIN) # $(JSMAIN)
MLFILES_PRE = \
	utils location driver io \
	types values env builtins ast ast_utils
MLFILES_POST = preprocess $(ANALYSES) typer interp
MLOTHER=cfg
MLIFILES =
SRCFILES = $(MLFILES_PRE:%=%.ml) \
	   $(MLFILES_POST:%=%.ml) \
	   $(MLOTHER:%=%.ml) \
	   $(MLIFILES:%=%.mli)

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

$(EXE): depend $(CMXFILES) $(EXEMAIN).cmx
	@$(PP) "Linking native code executable ..."
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) -linkpkg -o $@ $(CMXFILES) $(EXEMAIN).cmx

$(EXE).byt: depend $(CMIFILES) $(CMOFILES) $(EXEMAIN).cmo
	@$(PP) "Linking bytecode executable ..."
	$(OCAMLFIND) ocamlc -package $(PACKAGES)   \
	-linkpkg -o $@ $(CMOFILES) $(EXEMAIN).cmo

$(REPL): depend $(CMXFILES) $(REPLMAIN).cmx
	$(OCAMLFIND) ocamlopt -package $(PACKAGES) -linkpkg -o $@ $(CMXFILES) $(REPLMAIN).cmx

$(REPL).byt: depend $(CMIFILES) $(CMOFILES) $(REPLMAIN).cmo
	 $(OCAMLFIND) ocamlc -package $(PACKAGES) -linkpkg-o $@  $(CMOFILES) $(REPLMAIN).cmo

# Specific rules for web-related  code
ifneq ($(JSOFOCAML),no)
JSCAMLBYT = $(OCAMLFIND) ocamlc -package js_of_ocaml -package js_of_ocaml.syntax \
		-syntax camlp4o

$(JSMAIN).cmo: $(JSMAIN).ml
	$(PP) "Generating js main ..."
	$(JSCAMLBYT) -c $<

OPTIONS=-I ../www --file filemap.txt:/ --file ../tests:/

$(JS).byt: depend $(CMIFILES) $(JSCMOFILES) $(JSMAIN).cmo
	$(PP) "Generating bytecode for js file ..."
	$(JSCAMLBYT) -linkpkg -o $@ $(JSCMOFILES) $(JSMAIN).cmo

js: $(JS).byt
	$(PP) "Generating js file ..."
	$(JSOFOCAML) $< $(OPTIONS)
endif

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

ifneq ($(OCAMLMETRICS),no)
codestats:
	@echo "Word count"
	@wc -l $(SRCFILES)
	@$(OCAMLMETRICS) -h $(SRCFILES) > metrics.html
endif
