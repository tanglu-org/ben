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
open Types
open Baselib

type t = Types.expr

let of_string s =
  let lexbuf = Lexing.from_string s in
  Parser.full_expr Lexer.token lexbuf

let rec to_string = function
  | Match (f, r) ->
      sprintf "?%s ~ %s" f (string_of_regexp r)
  | Not e ->
      sprintf "!%s" (to_string e)
  | And (e1, e2) ->
      sprintf "(%s & %s)" (to_string e1) (to_string e2)
  | Or (e1, e2) ->
      sprintf "(%s | %s)" (to_string e1) (to_string e2)
  | Source -> "source"

let rec eval kind pkg = function
  | Match (field, (r, rex)) ->
      begin try
        let value = Package.get field pkg in
        ignore (Pcre.exec ~rex value);
        true
      with Not_found ->
        false
      end
  | Source ->
      kind = `source
  | Or (e1, e2) ->
      eval kind pkg e1 || eval kind pkg e2
  | And (e1, e2) ->
      eval kind pkg e1 && eval kind pkg e2
  | Not e ->
      not (eval kind pkg e)

let rec fields accu = function
  | Match (f, _) ->
      Fields.add f accu
  | Not e ->
      fields accu e
  | And (e1, e2) | Or (e1, e2) ->
      fields (fields accu e1) e2
  | Source ->
      accu
