(**************************************************************************)
(*  Copyright © 2009-2013 Stéphane Glondu <steph@glondu.net>              *)
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

module I = struct
  type t = int
  let compare = ( - )
end

module G = struct
  module V = struct
    type t = [`source] N.t
    let equal = (=)
    let hash = Hashtbl.hash
    let compare = Pervasives.compare
  end

  type t = ([`source], [`source] S.t) M.t

  let iter_vertex f graph = M.iter (fun k _ -> f k) graph
  let iter_succ f graph pkg = S.iter f (M.find pkg graph)
end

module C = Graph.Components.Make(G)
module T = Set.Make(I)

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

let compute_levels graph =
  let n, f = C.scc graph in
  let visited = Array.make n (-1) in
  let to_visit = Array.make n None in
  M.iter (fun node children ->
    let inode = f node in
    let c =
      match to_visit.(inode) with
      | None -> T.empty
      | Some c -> c
    in
    to_visit.(inode) <- Some (S.fold (fun child accu ->
      T.add (f child) accu
    ) children c)
  ) graph;
  Array.iteri (fun i x ->
    match x with
    | None -> failwith "error in SCC computation"
    | Some c -> to_visit.(i) <- Some (T.remove i c)
  ) to_visit;
  let rec visit node =
    let i = visited.(node) in
    if i >= 0 then i
    else (
      match to_visit.(node) with
      | None -> failwith "cycle detected in SCC graph"
      | Some children ->
        to_visit.(node) <- None;
        let i = T.fold (fun child i ->
          max i (visit child)
        ) children (-1) in
        let i = i + 1 in
        visited.(node) <- i;
        i
    )
  in
  for i = 0 to n - 1 do
    let (_ : int) = visit i in ()
  done;
  M.mapi (fun pkg _ -> visited.(f pkg)) graph

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
  let levels = compute_levels dgraph in
  let packages = List.map (fun (s, n) -> n, s) (M.bindings levels) in
  let packages = List.sort compare packages in
  lvlist_to_listlist 0 [] [] packages
