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

module S = Package.Set
let p = Stml_clflags.progress

let debcheck =
  let rex = Pcre.regexp "^([^ ]+) \\(= ([^)]+)\\): FAILED$" in
  fun filename ->
    let a, b = if !Stml_clflags.quiet then ("\n", "") else ("", "\n") in
    let ic = Printf.ksprintf
      Unix.open_process_in
      "edos-debcheck -quiet -failures < %s" filename
    in
    let rec loop accu =
      begin match (try Some (input_line ic) with End_of_file -> None) with
        | None ->
            accu
        | Some line ->
            begin try
              let r = Pcre.exec ~rex line in
              loop (S.add (Package.Name.of_string (Pcre.get_substring r 1)) accu)
            with Not_found ->
              Printf.eprintf "%sW: ignored line: %s%s%!" a line b;
              loop accu
            end
      end
    in
    let result = loop S.empty in
    begin match Unix.close_process_in ic with
      | Unix.WEXITED 0 -> ()
      | Unix.WEXITED i ->
          Printf.eprintf
            "%sW: subprocess edos-debcheck exited with code %d%s%!" a i b
      | Unix.WSIGNALED i ->
          Printf.eprintf
            "%sW: subprocess edos-debcheck died with signal %d%s%!" a i b
      | Unix.WSTOPPED i ->
          Printf.eprintf
            "%sW: subprocess edos-debcheck stopped with signal %d%s%!" a i b
    end; result

let parse_control_file kind filename to_keep f accu =
  let base = Filename.basename filename in
  let debcheck_data =
    if Stml_base.Fields.mem "edos-debcheck" to_keep then begin
      p "Running edos-debcheck on %s..." base;
      let result = debcheck filename in
      p "\n"; Some result
    end else None
  in
  p "Parsing %s..." base;
  let result =
    with_in_file filename begin fun ic ->
      Stml_lexer.stanza_fold to_keep begin fun name p accu ->
        f
          (Package.Name.of_string name)
          (Package.of_assoc ~debcheck_data kind p)
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
      Stml_parser.config_file Stml_lexer.token lexbuf
    with Stml_parser.Error ->
      let pos = Lexing.lexeme_start_p lexbuf in
      raise (Parsing_error
               (pos.pos_fname,
                pos.pos_lnum,
                pos.pos_cnum-pos.pos_bol))
  end
