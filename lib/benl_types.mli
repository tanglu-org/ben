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

type field = string
type regexp = string * Pcre.regexp

type comparison = Le | Lt | Eq | Gt | Ge

type expr =
  | EMatch of field * regexp
  | ENot of expr
  | EAnd of expr * expr
  | EOr of expr * expr
  | ESource
  | EList of expr list
  | EString of string
  | EVersion of comparison * string
  | EDep of string * string * (comparison * string) option
      (** field, package, comparison, reference_version *)

type config = (string * expr) list