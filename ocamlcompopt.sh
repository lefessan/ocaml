#!/bin/sh

#########################################################################
#                                                                       #
#                                 OCaml                                 #
#                                                                       #
#            Jacques Garrigue, Kyoto University RIMS                    #
#                                                                       #
#   Copyright 2002 Institut National de Recherche en Informatique et    #
#   en Automatique.  All rights reserved.  This file is distributed     #
#   under the terms of the Q Public License version 1.0.                #
#                                                                       #
#########################################################################

topdir=`dirname $0`

exec $topdir/boot/ocamlrun $topdir/ocamlopt -nostdlib -I $topdir/stdlib "$@"
