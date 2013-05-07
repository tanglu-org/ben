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

type error =
  | Illegal_escape of char
  | Unknown_error of exn
  | Nothing_to_download
  | Curl_error of int
  | Unexpected_char of string * char * int * int
  | Bad_marshalled_data of string
  | Unknown_command of string
  | Unknown_output_format of string
  | Unexpected_expression of string
  | Error_in_configuration_file of string
  | Missing_configuration_item of string
  | Unknown_configuration_item of string
  | Parsing_error of string * int * int
  | Template_not_found of string
  | Dynlink_error of Dynlink.error
(** The type of Ben-specific errors *)

exception Error of error
(** All Ben-specific errors are wrapped into this exception. *)

val string_of_error : error -> string
(** Return a human-readable explanation of an error. *)

val raise : error -> 'a
(** Wrapper around [Pervasives.raise] to raise a Ben exception. *)

val warn : error -> unit
(** Emit a warning. *)
