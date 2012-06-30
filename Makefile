##########################################################################
#  Copyright © 2009 Stéphane Glondu <steph@glondu.net>                   #
#                                                                        #
#  This program is free software: you can redistribute it and/or modify  #
#  it under the terms of the GNU Affero General Public License as        #
#  published by the Free Software Foundation, either version 3 of the    #
#  License, or (at your option) any later version, with the additional   #
#  exemption that compiling, linking, and/or using OpenSSL is allowed.   #
#                                                                        #
#  This program is distributed in the hope that it will be useful, but   #
#  WITHOUT ANY WARRANTY; without even the implied warranty of            #
#  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     #
#  Affero General Public License for more details.                       #
#                                                                        #
#  You should have received a copy of the GNU Affero General Public      #
#  License along with this program.  If not, see                         #
#  <http://www.gnu.org/licenses/>.                                       #
##########################################################################

# Configuration
NAME := ben
PREFIX := /usr/local

# Auto-detection
ifeq ($(OCAMLBEST),)
HAS_OPT := $(shell if which ocamlopt > /dev/null; then echo yes; fi)
else ifeq ($(OCAMLBEST),native)
HAS_OPT := yes
else
HAS_OPT :=
endif

CLASSIC := $(if $(INSIDE_EMACS),-classic-display)
ARCH := $(if $(HAS_OPT),native,byte)
OCAMLBUILD := ocamlbuild $(CLASSIC) $(if $(HAS_OPT),,-byte-plugin)
OCAMLBUILD_ENV :=

# Build
TARGETS := lib/benl.cma $(if $(HAS_OPT),lib/benl.cmxa) bin/$(NAME).$(ARCH) modules.dot
GENERATED := modules.png

# C stubs magic for bytecode
export CAML_LD_LIBRARY_PATH=$(CURDIR)/_build/lib

# Installation
BINDIR := $(DESTDIR)$(PREFIX)/bin

all: ocamlbuild $(GENERATED) doc

.PHONY: ocamlbuild doc clean env
ocamlbuild:
	$(OCAMLBUILD_ENV) $(OCAMLBUILD) $(TARGETS)

doc:
	$(MAKE) -C doc all

typerex: OCAMLBUILD_ENV := OCAMLFIND_COMMANDS='ocamlc=ocp-ocamlc ocamlopt=ocp-ocamlopt'
typerex: ocamlbuild

%.png: ocamlbuild
	dot -Tpng $(patsubst %.png,_build/%.dot,$@) > $@

clean:
	$(OCAMLBUILD) -clean
	rm -f *~ */*~ $(GENERATED)

env:
	@echo export CAML_LD_LIBRARY_PATH=$(CAML_LD_LIBRARY_PATH)

.PHONY: install
install:
	install -d $(BINDIR)
	install $(NAME).$(ARCH) $(BINDIR)/ben
	ocamlfind install $(NAME) $(wildcard $(addprefix _build/lib/,*.cmi *.mli *.cma *.cmx *.cmxa *.a *.so)) META
