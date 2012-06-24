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

open Benl_base
open Printf

open Benl_modules
module Marshal = Benl_marshal.Make(Marshallable)
open Marshallable

let sources_re = Pcre.regexp "Sources"
let is_source x =
  try
    ignore (Pcre.exec ~rex:sources_re x);
    true
  with Not_found -> false

let is_cache x =
  Benl_core.ends_with x ".cache"

let usage cmd =
  fprintf stderr "Usage: %s <query> [ file ... ]\n" cmd;
  exit 1

let help () =
  printf "    <query> [ file1 ... ]\n%!"

let main args =
  let query, files = match args with
    | query::files -> query, files
    | _ -> usage (sprintf "%s query" Sys.argv.(0))
  in
  let query = Query.of_string query in
  let caches, files = List.partition is_cache files in
  let sources, packages = List.partition is_source files in
  let sources = caches @ sources
  and packages = caches @ packages in
  let print kind filename =
    let keep = fun _ -> true in
    let eval = fun _ p ->
      if Query.eval kind p query then Package.print stdout p in
    let accu = fun n p () -> eval n p in
    match filename with
    | "-" ->
      Benl_utils.parse_control_in_channel kind "standard input" stdin keep accu ()
    | filename when Benl_core.ends_with filename ".gz" ->
      let ic = Unix.open_process_in ("zcat " ^ filename) in
      Benl_core.with_in_channel ic begin fun ic ->
        Benl_utils.parse_control_in_channel kind filename ic keep accu ()
      end
    | filename when Benl_core.ends_with filename ".bz2" ->
      let ic = Unix.open_process_in ("bzcat " ^ filename) in
      Benl_core.with_in_channel ic begin fun ic ->
        Benl_utils.parse_control_in_channel kind filename ic keep accu ()
      end
    | filename when is_cache filename ->
      let { src_map = srcs; bin_map = bins } = Marshal.load filename in
      begin match kind with
      | `binary -> PAMap.iter (Obj.magic eval) bins
      | `source -> Package.Map.iter (Obj.magic eval) srcs
      end
    | filename ->
      Benl_utils.parse_control_file kind filename keep accu ()
  in
  List.iter (print `source) sources;
  List.iter (print `binary) packages;;

let frontend = {
  Benl_frontend.name = "query";
  Benl_frontend.main = main;
  Benl_frontend.help = help;
}
