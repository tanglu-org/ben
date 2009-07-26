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
open Stml_core
open Stml_base
open Stml_types

module M = Package.Map
module S = Package.Set
module Name = Package.Name

module G = struct
  module V = struct
    type t = [`source] Name.t
    let equal = (=)
    let hash = Hashtbl.hash
  end

  type t = ([`source], [`source] S.t) M.t * ([`source], [`source] S.t) M.t

  let iter_vertex f (_, rdeps) = M.iter (fun k _ -> f k) rdeps
  let iter_succ f (deps, _) pkg = S.iter f (M.find pkg deps)
  let in_degree (_, rdeps) pkg = S.cardinal (M.find pkg rdeps)
end

module Topological = Graph.Topological.Make(G)

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

let invert_dep_graph src =
  M.mapi
    (fun pkg _ ->
       M.fold
         (fun name deps accu ->
            if S.mem pkg deps then
              S.add name accu
            else
              accu)
         src S.empty)
    src

let topo_split dgraph =
  let inverted = invert_dep_graph dgraph in
  let (a, b) =
    Topological.fold
      (fun name (local, accu) ->
         if S.exists (fun x -> S.mem x local) (M.find name dgraph) then
           (* already a dependency in this level -> switch to next level *)
           (S.add name S.empty, local::accu)
         else
           (* stay on the same level *)
           (S.add name local, accu))
      (inverted, dgraph)
      (S.empty, [])
  in
  List.rev_map S.elements (a::b)
