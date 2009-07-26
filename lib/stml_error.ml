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
  | Wget_error of int
  | Unexpected_char of char * int
  | Bad_marshalled_data of string
  | Unknown_command of string

exception Error of error

let string_of_exn = function
  | Illegal_escape c ->
      sprintf "illegal escape of %C" c
  | Unknown_error e ->
      sprintf "unexpected error: %s" (Printexc.to_string e)
  | Wget_error r ->
      sprintf "wget exited with return code %d" r
  | Nothing_to_download ->
      sprintf "nothing to download"
  | Unexpected_char (c, i) ->
      sprintf "unexpected char %C at position %d" c i
  | Bad_marshalled_data s ->
      sprintf "bad marshalled data in %s" s
  | Unknown_command s ->
      sprintf "unknown command: %s" s

let raise e = Pervasives.raise (Error e)
let not_found () = Pervasives.raise Not_found

let wrap f =
  try f ()
  with Error e -> eprintf "stm error: %s\n" (string_of_exn e)
