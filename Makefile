PKGS := -package unix,pcre
OCAMLC := ocamlfind ocamlc $(PKGS)
OCAMLOPT := ocamlfind ocamlopt $(PKGS)
OCAMLBUILD := \
  ocamlbuild -classic-display \
    -I lib \
    -ocamlc '$(OCAMLC)' -ocamlopt '$(OCAMLOPT)' \
    -lflags -linkpkg \
    -use-menhir -yaccflags --explain

TARGETS := bin/stmquery.native

all:
	$(OCAMLBUILD) $(TARGETS)

install:
	install -s _build/bin/stmquery.native /usr/local/bin/stmquery

clean:
	$(OCAMLBUILD) -clean
