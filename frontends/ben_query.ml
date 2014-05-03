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

let filters = ref []

let rec parse_local_args = function
  | "-s"::s::xs ->
      filters := Benl_core.simple_split ',' s;
      filters := List.map String.lowercase !filters;
      parse_local_args xs
  | x::xs -> x::(parse_local_args xs)
  | [] -> []

let help () =
  printf "    <query> [options] [ file1 ... ]\n%!";
  List.iter
    (fun (option , desc) ->
      Printf.printf "    %s: %s\n%!" option desc
    )
    [ "-s FIELD,FIELD,...", "Show only the body of these fields from the matching paragraphs";
    ]

let main args =
  let args = parse_local_args (Benl_frontend.parse_common_args args) in
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
    let keep = fun f ->
      let f = String.lowercase f in
      !filters = []
      || List.mem f !filters
      || List.mem f Benl_data.relevant_binary_keys
      || List.mem f Benl_data.relevant_source_keys
      || List.mem f !Benl_clflags.more_relevant_binary_keys
      || List.mem f !Benl_clflags.more_relevant_binary_keys
    in
    let eval e = fun _ p ->
      if e p query then Package.filter_print !filters stdout p in
    let accu = fun n p () -> eval (Query.eval kind) n p in
    match filename with
    | "-" ->
      Benl_utils.parse_control_in_channel kind "standard input" stdin keep accu ()
    | filename when Benl_compression.file_is_readable filename ->
      let tool = Benl_compression.display_tool
        (Benl_compression.of_string (FilePath.get_extension filename))
      in
      let ic = Unix.open_process_in (Printf.sprintf "%s %s" tool filename) in
      Benl_core.with_in_channel ic begin fun ic ->
        Benl_utils.parse_control_in_channel kind filename ic keep accu ()
      end
    | filename when is_cache filename ->
      let filename = Benl_clflags.get_cache_file ~name:filename () in
      let { src_map = srcs; bin_map = bins } = Marshal.load filename in
      begin match kind with
      | `binary -> PAMap.iter (eval Query.eval_binary) bins
      | `source -> Package.Map.iter (eval Query.eval_source) srcs
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
