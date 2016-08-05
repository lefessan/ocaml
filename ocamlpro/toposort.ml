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

    type topo_sort = {
        mutable running : bool;
      }

    type node = {
        mutable s_depth : int;
        mutable s_queued : bool;
        mutable s_sort : topo_sort;
      }

    let empty_sort = { running = false }
    let new_sort () = { running = false }
    let new_node () =
      { s_depth = 0;
        s_queued = false;
        s_sort = empty_sort;
      }


module Make(M : sig

      type t
      val node : t -> node
      val iter_deps : (t -> unit) -> t -> unit

    end) = (struct

      exception RecursiveDependency of M.t

          let sort list =
            let all = ref [] in
            let todo = ref [] in
            let sort = new_sort () in
            let nnodes = ref 0 in
            let queue t =
              let s = M.node t in
              if s.s_sort != sort then begin
                  s.s_sort <- sort;
                  s.s_queued <- false;
                  incr nnodes;
                  all := (t,s) :: !all
                end;
              if not s.s_queued then begin
                  s.s_queued <- true;
                  todo := t :: !todo
                end
            in

            List.iter queue list;
            let rec iter () =
              match !todo with
                [] -> ()
              | n1 :: tail ->
                  todo := tail;
                  let s1 = M.node n1 in
                  let dep_depth = s1.s_depth + 1 in
                  if dep_depth > 20 * !nnodes + 10 then
                    raise (RecursiveDependency n1);
                  M.iter_deps (fun d2 ->
                      let s2 = M.node d2 in
                      if s2.s_depth < dep_depth then begin
                          s2.s_depth <- dep_depth;
                          queue d2;
                        end
                  ) n1;
                  s1.s_queued <- false;
                  iter ()
            in
            iter ();

            let list = List.sort (fun (_,s1) (_,s2) ->
                  s2.s_depth - s1.s_depth) !all            in
            List.map fst list

        end)
