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
open Stml_marshal

module M = Package.Map
module S = Package.Set

let use_cache = ref false
let use_colors = ref false

type output_type = Text | Xhtml | Levels
let output_type = ref Levels

let p = Stml_clflags.progress
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
module Marshal = Stml_marshal.Make(Marshallable)
open Marshallable

let format_arch x =
  let f = match x with
    | Unknown -> (fun x -> " "^x^" ")
    | Up_to_date -> (fun x -> "("^x^")")
    | Outdated -> (fun x -> "["^x^"]")
  in
  let f =
    if !use_colors then
      match x with
        | Unknown -> f
        | Up_to_date -> (fun x -> "\027[32m"^(f x)^"\027[0m")
        | Outdated -> (fun x -> "\027[1m\027[31m"^(f x)^"\027[0m")
    else f
  in f

let parse_binaries accu arch =
  p "Parsing Packages.%s..." arch;
  let res = Stml_utils.parse_control_file
    (!Stml_clflags.cache_dir // ("Packages."^arch))
    !!to_keep `binary
    (fun name pkg accu ->
       if Query.eval_binary pkg !!is_affected then
         PAMap.add (name, arch) pkg accu
       else accu)
    accu
  in p "\n"; res

let parse_sources accu =
  p "Parsing sources...";
  let res = Stml_utils.parse_control_file
    (!Stml_clflags.cache_dir // "Sources")
    !!to_keep `source
    (fun name pkg accu ->
       if Query.eval_source pkg !!is_affected then
         M.add name pkg accu
       else accu)
    accu
  in p "\n"; res

let get_data () =
  let file = !Stml_clflags.cache_dir // "monitor.cache" in
  if !use_cache then
    Marshal.load file
  else
    let data = {
      src_map = parse_sources M.empty;
      bin_map =
        List.fold_left
          parse_binaries PAMap.empty !Stml_clflags.architectures;
    } in
    Marshal.dump file data;
    data

let print_dep_line src deps =
  printf "%s:" !!!src;
  S.iter (fun dep -> printf " %s" !!!dep) deps;
  printf "\n%!"

let print_dep_graph x = M.iter print_dep_line x

let rec parse_local_args = function
  | "--use-cache"::xs ->
      use_cache := true;
      parse_local_args xs
  | "--color"::xs ->
      use_colors := true;
      output_type := Text;
      parse_local_args xs
  | "--text"::xs ->
      output_type := Text;
      parse_local_args xs
  | "--html"::xs ->
      output_type := Xhtml;
      parse_local_args xs
  | x::xs -> x::(parse_local_args xs)
  | [] -> []

let compute_monitor_data sources binaries rounds =
  List.map begin fun xs ->
    let packages = List.sort (fun x y -> compare !!!x !!!y) xs in
    List.map begin fun src ->
      let states =
        List.map begin fun arch ->
          let pkgs = Package.binaries (M.find src sources) in
          List.fold_left begin fun accu pkg ->
            try
              let pkg = PAMap.find (pkg, arch) binaries in
              if accu = Outdated || Query.eval_binary pkg !!is_bad then
                Outdated
              else if Query.eval_binary pkg !!is_good then
                Up_to_date
              else Unknown
            with Not_found -> accu
          end Unknown pkgs
        end !Stml_clflags.architectures
      in src, states
    end packages
  end rounds

let print_text_monitor sources binaries rounds =
  let monitor_data = compute_monitor_data sources binaries rounds in
  let nmax = M.fold begin fun src _ accu ->
    let n = String.length !!!src in
    if n > accu then n else accu
  end sources 0 in
  let src_fmt = Scanf.format_from_string (sprintf "%%%ds:" (nmax+2)) "%s" in
  let width =
    String.length (String.concat "   " !Stml_clflags.architectures)+6+nmax
  in
  let nrounds = String.length (string_of_int (List.length rounds)) in
  let hwidth = String.length "> Dependency level  <" + nrounds in
  let header_fmt =
    let width = width-hwidth+2 in
    let left = width/2 in
    let right = width-left in
    let buf = Buffer.create 64 in
    for i = 1 to left do Buffer.add_char buf '=' done;
    bprintf buf "> Dependency level %%%dd <" nrounds;
    for i = 1 to right do Buffer.add_char buf '=' done;
    Buffer.add_char buf '\n';
    Scanf.format_from_string (Buffer.contents buf) "%d"
  in
  list_iteri begin fun i xs ->
    printf header_fmt i;
    List.iter begin fun (src, states) ->
      printf src_fmt !!!src;
      List.iter begin fun (arch, state) ->
        printf " %s" (format_arch state arch)
      end (List.combine !Stml_clflags.architectures states);
      printf "\n";
    end xs;
    printf "\n"
  end monitor_data

let print_dependency_levels dep_graph rounds =
  list_iteri begin fun i xs ->
    printf "===[ Dependency level %d ]=====\n" i;
    let packages = List.sort (fun x y -> compare !!!x !!!y) xs in
    List.iter begin fun src ->
      let deps = M.find src dep_graph in
      print_dep_line src deps
    end packages
  end rounds

let main args =
  let _ = parse_local_args (Stml_plugin.parse_common_args args) in
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
  match !output_type with
    | Levels -> print_dependency_levels dep_graph rounds
    | Text -> print_text_monitor sources binaries rounds
    | Xhtml -> assert false

let subcommand = {
  Stml_plugin.name = "monitor";
  Stml_plugin.main = main;
}
