#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Xavier Leroy, projet Cristal, INRIA Rocquencourt           #
#                                                                       #
#   Copyright 1999 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

include Makefile.shared

# To make custom toplevels

ocamlmktop: ocamlmktop.tpl ../config/Makefile
	if ${OCP_RELOCATABLE}; then \
	  sed -e 's|%%BINDIR%%|$$(dirname $$0)|' ocamlmktop.tpl > ocamlmktop; \
        else \
	  sed -e 's|%%BINDIR%%|$(BINDIR)|' ocamlmktop.tpl > ocamlmktop; \
        fi
	chmod +x ocamlmktop

install::
	cp ocamlmktop $(INSTALL_BINDIR)

clean::
	rm -f ocamlmktop
