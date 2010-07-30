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

type error =
  | Illegal_escape of char
  | Unknown_error of exn
  | Nothing_to_download
  | Curl_error of int
  | Unexpected_char of string * char * int * int
  | Bad_marshalled_data of string
  | Unknown_command of string
  | Unexpected_expression of string
  | Error_in_configuration_file of string
  | Missing_configuration_item of string
  | Parsing_error of string * int * int

exception Error of error

let string_of_exn = function
  | Illegal_escape c ->
      sprintf "illegal escape of %C" c
  | Unknown_error e ->
      sprintf "unexpected error: %s" (Printexc.to_string e)
  | Curl_error r ->
      sprintf "curl exited with return code %d" r
  | Nothing_to_download ->
      sprintf "nothing to download"
  | Unexpected_char (file, c, line, column) ->
      sprintf
        "unexpected char %C in file %S, line %d, position %d"
        c file line column
  | Bad_marshalled_data s ->
      sprintf "bad marshalled data in %s" s
  | Unknown_command s ->
      sprintf "unknown command: %s" s
  | Unexpected_expression s ->
      sprintf "unexpected expression: %s" s
  | Error_in_configuration_file s ->
      sprintf "error in configuration file: %s" s
  | Missing_configuration_item s ->
      sprintf "missing configuration item: %s" s
  | Parsing_error (file, line, column) ->
      sprintf
        "parse error in file %S, line %d, character %d"
        file line column

let raise e = Pervasives.raise (Error e)
let not_found () = Pervasives.raise Not_found

let wrap f =
  try f ()
  with Error e -> eprintf "ben error: %s\n" (string_of_exn e)
