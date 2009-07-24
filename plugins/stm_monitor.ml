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
open Corelib
open Baselib
open Stmlib
open Marshalling

module M = Package.Map
module S = Package.Set

let use_cache = ref false

let p = Clflags.progress
let ( // ) = Filename.concat
let ( !! ) = Lazy.force
let ( !!! ) = Package.Name.to_string

let is_affected = lazy (Query.of_string ".maintainer~/debian-ocaml-maint/ | .build-depends~/ocaml/ | .build-depends-indep~/ocaml/ | .depends~/ocaml(-base)?(-nox)?-3\\.11\\../")
let is_good = lazy (Query.of_string ".depends ~ /ocaml(-base)?(-nox)?-3\\.11\\.1/")
let is_bad = lazy (Query.of_string ".depends ~ /ocaml(-base)?(-nox)?-3\\.11\\.0/")
let to_keep =
  lazy begin
    let (@@) x y = Query.fields y x in
    !!is_affected @@ !!is_good @@ !!is_bad @@ core_fields
  end

(** Basename of all files handled by this script *)
let basename = "ocaml_transition_monitor"

let src_webbrowse_url =
  "http://git.debian.org/?p=users/glondu-guest/stm.git"

module PAIndex = struct
  type t = [`binary] Package.Name.t * string
  let compare = Pervasives.compare
end
module PAMap = Map.Make(PAIndex)

module Marshallable = struct
  let magic_number = "STMA0901"
  type t = {
    src_map : ([`source], [`source] Package.t) Package.Map.t;
    bin_map : [`binary] Package.t PAMap.t
  }
end
module Marshal = Marshalling.Make(Marshallable)
open Marshallable

let parse_binaries accu arch =
  p "Parsing Packages.%s..." arch;
  let res = Utils.parse_control_file
    (!Clflags.cache_dir // ("Packages."^arch))
    !!to_keep `binary
    (fun name pkg accu ->
       if Query.eval_binary pkg !!is_affected then
         PAMap.add (name, arch) pkg accu
       else accu)
    accu
  in p "\n"; res

let parse_sources accu =
  p "Parsing sources...";
  let res = Utils.parse_control_file
    (!Clflags.cache_dir // "Sources")
    !!to_keep `source
    (fun name pkg accu ->
       if Query.eval_source pkg !!is_affected then
         M.add name pkg accu
       else accu)
    accu
  in p "\n"; res

let get_data () =
  let file = !Clflags.cache_dir // "monitor.cache" in
  if !use_cache then
    Marshal.load file
  else
    let data = {
      src_map = parse_sources M.empty;
      bin_map =
        List.fold_left parse_binaries PAMap.empty !Clflags.architectures;
    } in
    Marshal.dump file data;
    data

let print_dep_line src deps =
  printf "%s:" (Package.Name.to_string src);
  S.iter (fun dep -> printf " %s" (Package.Name.to_string dep)) deps;
  printf "\n%!"

let print_dep_graph x = M.iter print_dep_line x

let rec parse_local_args = function
  | "--use-cache"::xs ->
      use_cache := true;
      parse_local_args xs
  | x::xs -> x::(parse_local_args xs)
  | [] -> []

let main args =
  let _ = parse_local_args (Stmpluginlib.parse_common_args args) in
  let {src_map = sources; bin_map = binaries} = get_data () in
  let src_of_bin : ([`binary], [`source] Package.Name.t) M.t =
    PAMap.fold
      (fun (name, _) pkg accu ->
         let source = Package.get "source" pkg in
         M.add name (Package.Name.of_string source) accu)
      binaries
      M.empty
  in
  let src_of_bin =
    M.fold
      (fun name pkg accu ->
         List.fold_left
           (fun accu bin -> M.add bin name accu)
           accu
           (Package.binaries pkg))
      sources
      src_of_bin
  in
  let dep_graph = Dependencies.get_dep_graph sources src_of_bin in
  let rounds = Dependencies.topo_split dep_graph in
(*
  print_dep_graph dep_graph;
*)
  list_iteri
    (fun i xs ->
       printf "===> Dependency level %d <===\n" i;
       let packages = List.sort (fun x y -> compare !!!x !!!y) xs in
       List.iter
         (fun src ->
            let deps = M.find src dep_graph in
            print_dep_line src deps)
         packages;
       printf "\n")
    rounds;
  ()

let subcommand = {
  Stmpluginlib.name = "monitor";
  Stmpluginlib.main = main;
}
