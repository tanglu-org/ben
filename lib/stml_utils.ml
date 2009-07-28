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

open Stml_core
open Stml_error
open Lexing

let parse_control_file filename to_keep
    (kind : 'a)
    (f : 'a Package.Name.t -> 'a Package.t -> 'b -> 'b)
    (accu : 'b) : 'b =
  with_in_file filename begin fun ic ->
    Stml_lexer.stanza_fold to_keep
      (fun name p accu -> f name p accu)
      (from_channel ic)
      accu
  end

let parse_config_file filename =
  with_in_file filename begin fun ic ->
    let lexbuf = from_channel ic in
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <- { pos with pos_fname = filename };
    try
      Stml_parser.config_file Stml_lexer.token lexbuf
    with Stml_parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      raise (Parsing_error
               (pos.pos_fname,
                pos.pos_lnum,
                pos.pos_cnum-pos.pos_bol))
  end
