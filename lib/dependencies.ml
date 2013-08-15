(**************************************************************************)
(*  Copyright © 2009 Stéphane Glondu <steph@glondu.net>                   *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Affero General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version, with the additional   *)
(*  exemption that compiling, linking, and/or using OpenSSL is allowed.   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Affero General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Affero General Public      *)
(*  License along with this program.  If not, see                         *)
(*  <http://www.gnu.org/licenses/>.                                       *)
(**************************************************************************)

open Printf
open Benl_core
open Benl_base
open Benl_types

module M = Package.Map
module S = Package.Set
module N = Package.Name

let get_dep_graph src bin =
  M.mapi
    (fun name pkg ->
       let deps = Package.build_depends pkg in
       List.fold_left
         (fun accu dep ->
            try S.add (M.find dep bin) accu
            with Not_found -> accu)
         S.empty
         deps)
    src

(** Generate a pseudo-package. *)
let newcycle cycle =
  (* TODO: we should use a proper algebraic type for that... *)
  N.of_string ("_" ^ Marshal.to_string (S.elements cycle) [])

(** Replace all packages in [cycle] by a pseudo-package. *)
let collapse cycle graph =
  assert (S.cardinal cycle > 1);
  let repr = newcycle cycle in
  let deps, graph = M.fold (fun x deps (d, g) ->
    let deps = S.fold (fun x accu ->
      S.add (if S.mem x cycle then repr else x) accu
    ) deps S.empty in
    if S.mem x cycle then
      S.union d deps, g
    else
      d, M.add x deps g
  ) graph (S.empty, M.empty) in
  M.add repr deps graph

let find x m = try Some (M.find x m) with Not_found -> None

(** In principle, return the level of [node]. It is computed with a
    depth-first browse of the dependency graph. [visited] maps visited
    nodes to their level, [to_visit] maps unvisited nodes to their
    children. [graph] and [path] are used in case a cycle is detected.

    The return type is a bit complicated because of the handling of
    cycles:
    - [`Partial (i, visited, to_visit)] means that the level has been
      successfully computed, and returns said level and updated
      visited and to_visit maps;
    - [`Full visited] means that a cycle has been detected. In this
      case, the cycle is collapsed into a single node, and the
      resulting graph is visited again; the resulting visited map is
      then returned.
*)
let rec visit_node graph path node visited to_visit =
  match find node visited with
  | Some i -> `Partial (i, visited, to_visit)
  | None ->
    match find node to_visit with
    | None ->
      Printf.eprintf "W: non-trivial cycle detected: %S" (N.to_string node);
      let rec collect_cycle accu = function
        | [] -> failwith "could not reconstruct a cycle"
        | x :: xs ->
          Printf.eprintf " -> %S" (N.to_string x);
          let accu = S.add x accu in
          if x = node then accu else collect_cycle accu xs
      in
      let cycle = collect_cycle S.empty path in
      Printf.eprintf "\n%!";
      visit_graph (collapse cycle graph)
    | Some children ->
      (* this is the first time we visit this node *)
      let path = node :: path in
      let to_visit = M.remove node to_visit in
      let children = S.remove node children in
      let rec loop i visited to_visit = function
        | [] ->
          let i = i + 1 in
          let visited = M.add node i visited in
          `Partial (i, visited, to_visit)
        | x :: xs ->
          match visit_node graph path x visited to_visit with
          | `Partial (j, visited, to_visit) ->
            loop (max i j) visited to_visit xs
          | full -> full
      in loop (-1) visited to_visit (S.elements children)

(** Compute the levels of all nodes of [graph]. *)
and visit_graph graph =
  let rec loop visited to_visit =
    match (try Some (M.choose to_visit) with Not_found -> None) with
    | None -> `Full visited
    | Some (node, _) ->
      match visit_node graph [] node visited to_visit with
      | `Partial (_, visited, to_visit) -> loop visited to_visit
      | full -> full
  in loop M.empty graph

let rev_cons_if_not_empty xs ys =
  match xs with
  | [] -> ys
  | _ :: _ -> List.rev xs :: ys

let rec lvlist_to_listlist last accu result = function
  | [] ->
    List.rev (rev_cons_if_not_empty accu result)
  | (i, pkg) :: xs ->
    if i = last then
      lvlist_to_listlist i (pkg :: accu) result xs
    else
      lvlist_to_listlist i [pkg] (rev_cons_if_not_empty accu result) xs

let topo_split dgraph =
  match visit_graph dgraph with
  | `Full levels ->
    let packages = List.map (fun (s, n) ->
      let rec extract s =
        (* FIXME: unsafe code *)
        let s' = N.to_string s in
        if s'.[0] = '_' then
          List.flatten (List.map extract (Marshal.from_string s' 1))
        else [n, s]
      in extract s
    ) (M.bindings levels) in
    let packages = List.sort compare (List.flatten packages) in
    lvlist_to_listlist 0 [] [] packages
  | _ -> failwith "unexpected failure in computation of dependency levels"
