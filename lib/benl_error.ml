(**************************************************************************)
(*  Copyright © 2009-2013 Stéphane Glondu <steph@glondu.net>              *)
(*            © 2010-2013 Mehdi Dogguy <mehdi@dogguy.org>                 *)
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
  | Unknown_output_format of string
  | Unknown_input_format of string
  | Unexpected_expression of string
  | Missing_configuration_file
  | Error_in_configuration_file of string
  | Missing_configuration_item of string
  | Unknown_configuration_item of string
  | Parsing_error of string * int * int
  | Template_not_found of string
  | Dynlink_error of Dynlink.error

exception Error of error

let string_of_error = function
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
  | Unknown_output_format s ->
      sprintf "unknown output format: %s" s
  | Unknown_input_format s ->
      sprintf "unknown input format: %s" s
  | Unexpected_expression s ->
      sprintf "unexpected expression: %s" s
  | Missing_configuration_file ->
      sprintf "No configuration file has been specified"
  | Error_in_configuration_file s ->
      sprintf "error in configuration file: %s" s
  | Missing_configuration_item s ->
      sprintf "missing configuration item: %s" s
  | Unknown_configuration_item s ->
      sprintf "unknown configuration item: %s" s
  | Parsing_error (file, line, column) ->
      sprintf
        "parse error in file %S, line %d, character %d"
        file line column
  | Template_not_found name ->
      sprintf "template %s not found" name
  | Dynlink_error e ->
      sprintf "Dynlink error: %s" (Dynlink.error_message e)

let () =
  Printexc.register_printer
    (function
      | Error exn -> Some ("ben-specific error: " ^ (string_of_error exn))
      | _ -> None
    )

let raise e = Pervasives.raise (Error e)
let warn e = Printf.eprintf "W: %s\n%!" (string_of_error e)

let warn_exn msg e =
  Printf.eprintf "W: %s: %s\n%!" msg (Printexc.to_string e)

let error_exn msg e =
  Printf.eprintf "E: %s: %s\n" msg (Printexc.to_string e);
  Printexc.print_backtrace stderr
