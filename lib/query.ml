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
open Benl_error
open Benl_types
open Benl_base

type t = Benl_types.expr

let of_expr x = x

let of_string s =
  let lexbuf = Lexing.from_string s in
  Benl_parser.full_expr Benl_lexer.token lexbuf

let parens show expr =
  if show
  then sprintf "(%s)" expr
  else expr

let rec to_string_b ?(escape = true) last_op = function
  | EMatch (f, ERegexp r) ->
      sprintf ".%s ~ %s" f (string_of_regexp r)
  | ENot e ->
      sprintf "!%s" (to_string_b "!" e)
  | Etrue -> sprintf "true"
  | Efalse -> sprintf "false"
  | EAnd (e1, e2) ->
    parens
      (last_op <> "&" && last_op <> "")
      (sprintf "%s & %s" (to_string_b "&" e1) (to_string_b "&" e2))
  | EOr (e1, e2) ->
    parens
      (last_op <> "|" && last_op <> "")
      (sprintf "%s | %s" (to_string_b "|" e1) (to_string_b "|" e2))
  | EList xs ->
      sprintf "[%s]" (String.concat "; " (List.map (to_string_b "") xs))
  | ESource -> "source"
  | EString x -> string_of_string escape x
  | EVersion (cmp, x) ->
    parens (last_op <> "") (sprintf "%s \"%s\"" (string_of_cmp cmp) x)
  | EMatch (field, EDep (package, cmp, ref_version)) ->
    parens (last_op <> "") (sprintf "%s ~ \"%s\" %s \"%s\""
                field
                package
                (string_of_cmp cmp)
                ref_version)
  | EMatch (field, EString package) ->
    parens (last_op <> "") (sprintf "%s ~ \"%s\"" field package)
  | x -> raise (Unexpected_expression "<unable to convert to string>")

let to_string ?(escape = true) = to_string_b ~escape ""

let rec eval kind pkg = function
  | EMatch (field, ERegexp (r, rex)) ->
      begin try
        let value = Package.get field pkg in
        ignore (Pcre.exec ~rex value);
        true
      with Not_found ->
        false
      end
  | Etrue -> true
  | Efalse -> false
  | ESource ->
      kind = `source
  | EOr (e1, e2) ->
      eval kind pkg e1 || eval kind pkg e2
  | EAnd (e1, e2) ->
      eval kind pkg e1 && eval kind pkg e2
  | ENot e ->
      not (eval kind pkg e)
  | EVersion (cmp, ref_version) ->
      let value = Package.get "version" pkg in
      version_compare cmp value ref_version
  | EMatch (field, EDep (package, cmp, refv)) ->
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
  | EMatch (field, EString package) ->
    let deps = Package.dependencies field pkg in
    List.exists
      (fun x -> x.Package.dep_name = package)
      deps
  | x ->
      raise (Unexpected_expression (to_string x))

let eval_source x = eval `source x
let eval_binary x = eval `binary x

let rec fields accu = function
  | EMatch (f, _) ->
      Fields.add f accu
  | ENot e ->
      fields accu e
  | Etrue | Efalse -> accu
  | EAnd (e1, e2) | EOr (e1, e2) ->
      fields (fields accu e1) e2
  | EList xs ->
      List.fold_left fields accu xs
  | ESource | EString _ | ERegexp _ | EDep _ ->
      accu
  | EVersion _ ->
      Fields.add "version" accu
