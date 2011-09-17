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

let parse_control_file kind filename to_forget f accu =
  let base = Filename.basename filename in
  p "Parsing %s..." base;
  let result =
    with_in_file filename begin fun ic ->
      Benl_lexer.stanza_fold to_forget begin fun name p accu ->
        f
          (Package.Name.of_string name)
          (Package.of_assoc kind p)
          accu
      end (from_channel ic) accu
    end
  in p "\n"; result

let parse_config_file filename =
  with_in_file filename begin fun ic ->
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
  end

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
