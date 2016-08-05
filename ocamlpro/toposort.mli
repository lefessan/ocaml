(***********************************************************************)
(*                                                                     *)
(*                              ocp-memprof                            *)
(*                                                                     *)
(*  Copyright 2014, Fabrice Le Fessant, INRIA/OCamlPro.                *)
(*                    All rights reserved.                             *)
(*  All rights reserved.  This file is distributed under the terms     *)
(*  of the Q Public License version 1.0.                               *)
(*                                                                     *)
(***********************************************************************)

(** Simple Topological Sort

This module provides a simple way to sort in topological order
a set of nodes.
*)

type node
(** The abstract type of a node in the graph *)

val new_node : unit -> node
(** [new_node ()] returns a new node in the graph *)

module Make :functor (M : sig
    (** [Make(M)] returns a module providing a function [sort]
        to sort in topological order a list of values of type [M.t].
        To sort its input, it requires a function [M.node] providing,
        for a given value, the uniq associated node (obtained with
        [new_node] during the construction of the value), and a
        function [M.iter_deps] that applies its first argument (a
        function) on all the dependencies of its second argument.
        [sort] may raise [RecursiveDependency n] where [n] is a
        value in the middle of a dependency loop.
    *)

    type t
    val node : t -> node

    val iter_deps : (t -> unit) -> t -> unit
  end) ->
      sig
        exception RecursiveDependency of M.t
        val sort : M.t list -> M.t list
      end
