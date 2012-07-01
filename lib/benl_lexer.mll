(**************************************************************************)
(*  Copyright © 2009-2010 Stéphane Glondu <steph@glondu.net>              *)
(*            © 2010 Mehdi Dogguy <mehdi@dogguy.org>                      *)
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

{
  open Benl_error
  open Benl_base
  open Benl_parser

  let id_from_token s =
    match (String.lowercase s) with
      | "true" -> TRUE
      | "false" -> FALSE
      | _ -> IDENT s

}

let space = [' ' '\t']
let field_name = ['a'-'z' 'A'-'Z' '-' '_' '0'-'9']+
let field_value = ([^ '\n'] | '\n' space)*

rule stanza empty accu = parse
  | (field_name as name) space* ":" space* (field_value as value) '\n'?
      {
        let name = String.lowercase name in
        stanza false ((name, value)::accu) lexbuf
      }
  | '\n'+ | eof
        {
          if empty then Pervasives.raise End_of_file
          else List.rev accu
        }

and token = parse
  | '.' (field_name as name) { FIELD name }
  | "source" { SOURCE }
  | '=' { EQ }
  | "<=" { LE }
  | ("<" | "<<") { LT }
  | ">=" { GE }
  | (">" | ">>") { GT }
  | '~' { MATCH }
  | '|' { OR }
  | '&' { AND }
  | '!' { NOT }
  | '(' { LPAREN }
  | ')' { RPAREN }
  | '[' { LBRACKET }
  | ']' { RBRACKET }
  | ';' { SEMICOLON }
  | field_name as id { id_from_token id }
  | '#' { comment lexbuf }
  | ('"'|"'") as c { STRING (string c (Buffer.create 128) lexbuf) }
  | '@' (_ as c) | ('/' as c) { REGEXP (regexp c (Buffer.create 32) lexbuf) }
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | space { token lexbuf }
  | _ as c
      {
        let pos = Lexing.lexeme_start_p lexbuf in
        raise (Unexpected_char (pos.Lexing.pos_fname,
                                c,
                                pos.Lexing.pos_lnum,
                                pos.Lexing.pos_cnum-pos.Lexing.pos_bol))
      }
  | eof { EOF }

and regexp separator buf = parse
  | _ as c
      {
        if c = separator then
          let res = Buffer.contents buf in
          let reg = Pcre.regexp res in
          (res, reg)
        else begin
          Buffer.add_char buf c;
          if c = '\n' then Lexing.new_line lexbuf;
          regexp separator buf lexbuf
        end
      }

and string separator buf = parse
  | _ as c
      {
        if c = separator then
          Buffer.contents buf
        else begin
          Buffer.add_char buf c;
          if c = '\n' then Lexing.new_line lexbuf;
          string separator buf lexbuf
        end
      }

and comment = parse
  | '\n' { Lexing.new_line lexbuf; token lexbuf }
  | _ { comment lexbuf }

{
  let stanza_fold f lexbuf accu =
    let rec loop accu =
      let stanza =
        try Some (stanza true [] lexbuf)
        with End_of_file -> None
      in
      match stanza with
        | None -> accu
        | Some x -> loop
            (f x accu)
    in loop accu
}
