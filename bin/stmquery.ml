(**************************************************************************)
(*  Copyright © 2009 Stéphane Glondu <steph@glondu.net>                 *)
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

open Baselib
open Printf
open Utils

let sources_re = Pcre.regexp "Sources"
let is_source x =
  try
    ignore (Pcre.exec ~rex:sources_re x);
    true
  with Not_found -> false

let usage cmd =
  fprintf stderr "Usage: %s <query> [ file ... ]\n" cmd;
  exit 1

let main () =
  let query, files = match Array.to_list Sys.argv with
    | [] -> usage "tmquery"
    | _::query::files -> query, files
    | x::_ -> usage x
  in
  let query = Query.of_string query in
  let to_keep = Query.fields core_fields query in
  let sources, packages = List.partition is_source files in
  let print is_source ic =
    let lexbuf = Lexing.from_channel ic in
    Lexer.stanza_fold to_keep
      (fun () p -> if Query.eval is_source p query then print_package p)
      ()
      lexbuf
  in
  List.iter (fun x -> with_in_file x (print true)) sources;
  List.iter (fun x -> with_in_file x (print false)) packages;;

let _ = wrap main
