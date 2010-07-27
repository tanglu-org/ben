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
open Stml_error
open Stml_types
open Stml_base

type t = Stml_types.expr

let of_expr x = x

let of_string s =
  let lexbuf = Lexing.from_string s in
  Stml_parser.full_expr Stml_lexer.token lexbuf

let rec to_string = function
  | EMatch (f, r) ->
      sprintf ".%s ~ %s" f (string_of_regexp r)
  | ENot e ->
      sprintf "!%s" (to_string e)
  | EAnd (e1, e2) ->
      sprintf "(%s & %s)" (to_string e1) (to_string e2)
  | EOr (e1, e2) ->
      sprintf "(%s | %s)" (to_string e1) (to_string e2)
  | EList xs ->
      sprintf "[%s]" (String.concat "; " (List.map to_string xs))
  | ESource -> "source"
  | EString x -> string_of_string x
  | EVersion (cmp, x) -> sprintf "(%s \"%s\")" (string_of_cmp cmp) x
  | EDep (field, package, Some (cmp, ref_version)) ->
    sprintf "(%s %% (\"%s\" %s \"%s\"))" field package (string_of_cmp cmp) ref_version
  | EDep (field, package, None) ->
    sprintf "(%s %% (\"%s\"))" field package

let rec eval kind pkg = function
  | EMatch (field, (r, rex)) ->
      begin try
        let value = Package.get field pkg in
        ignore (Pcre.exec ~rex value);
        true
      with Not_found ->
        false
      end
  | ESource ->
      kind = `source
  | EOr (e1, e2) ->
      eval kind pkg e1 || eval kind pkg e2
  | EAnd (e1, e2) ->
      eval kind pkg e1 && eval kind pkg e2
  | ENot e ->
      not (eval kind pkg e)
  | (EString _ | EList _) as x ->
      raise (Unexpected_expression (to_string x))
  | EVersion (cmp, ref_version) ->
      let value = Package.get "version" pkg in
      version_compare cmp value ref_version
  | EDep (field, package, Some (cmp, refv)) ->
    let deps = Package.dependencies field pkg in
    List.exists
      (fun x ->
        x.Package.dep_name = package && begin
          match x.Package.dep_version with
            | None -> false
            | Some (rcmp, rrefv) ->
              match rcmp, cmp with
                | Ge, Ge | Gt, Ge | Gt, Gt -> Version.compare rrefv refv >= 0
                | Ge, Gt -> Version.compare rrefv refv > 0
                | _, _ -> false  (* FIXME: missing cases *)
        end)
      deps
  | EDep (field, package, None) ->
    let deps = Package.dependencies field pkg in
    List.exists
      (fun x -> x.Package.dep_name = package)
      deps

let eval_source x = eval `source x
let eval_binary x = eval `binary x

let rec fields accu = function
  | EMatch (f, _) ->
      Fields.add f accu
  | ENot e ->
      fields accu e
  | EAnd (e1, e2) | EOr (e1, e2) ->
      fields (fields accu e1) e2
  | EList xs ->
      List.fold_left fields accu xs
  | ESource | EString _ ->
      accu
  | EVersion _ ->
      Fields.add "version" accu
  | EDep (field, _, _) ->
    Fields.add field accu
