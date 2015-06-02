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

open Benl_core
open Benl_error
open Lexing

module S = Package.Set
let p = Benl_clflags.progress

let parse_control_in_channel kind filename ic keep f accu =
  p "Parsing %s..." filename;
  let result = Benl_lexer.stanza_fold begin fun p accu ->
    let p = StringMap.filter (fun k v -> keep k) p in
    f (Package.Name.of_string (StringMap.find "package" p))
      (Package.of_assoc kind p)
      accu
    end (from_channel ic) accu
  in p "\n"; result

let parse_control_file kind filename keep f accu =
  let base = Filename.basename filename in
  with_in_file filename begin fun ic ->
    parse_control_in_channel kind base ic keep f accu
  end

let parse_config_from_in_channel ?(filename = "stdin") ic =
  let lexbuf = from_channel ic in
  let pos = lexbuf.lex_curr_p in
  lexbuf.lex_curr_p <- { pos with pos_fname = filename };
  try
    Benl_parser.config_file Benl_lexer.token lexbuf
  with Benl_parser.Error ->
    let pos = Lexing.lexeme_start_p lexbuf in
    raise (Parsing_error
             (pos.pos_fname,
              pos.pos_lnum,
              pos.pos_cnum-pos.pos_bol))

let parse_config_file filename =
  with_in_file filename (parse_config_from_in_channel ~filename)

let file_content file =
  let lines = ref "" in
  let inchan = open_in file in
  try
    while true; do
      lines := Printf.sprintf "%s%s\n" !lines (input_line inchan)
    done; ""
  with End_of_file ->
    close_in inchan;
    !lines

let dump_to_file name string =
  let outchan = open_out name in
  let () = output_string outchan string in
  close_out outchan

let dump_xhtml_to_file filename xhtml =
  let outchan = open_out filename in
  let () = Xhtml.P.print (output_string outchan) xhtml in
  close_out outchan
