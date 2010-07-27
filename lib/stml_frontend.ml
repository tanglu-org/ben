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

open Stml_error
open Stml_types

type frontend = {
  name : string;
  main : string list -> unit
}
let frontends = ref []

let register_frontend sc =
  frontends := (sc.name, sc) :: !frontends

let get_frontend x =
  try List.assoc x !frontends
  with Not_found -> raise (Unknown_command x)

let to_cmd x =
  let n = String.length x in
  if n > 4 && String.sub x 0 4 = "stm-" then
    String.sub x 4 (n-4)
  else x

let fail fmt = Printf.ksprintf
  (fun x -> raise (Error_in_configuration_file x))
  fmt

let check_string what = function
  | EString s -> s
  | _ -> fail "%s must be a string" what

let check_string_list what = function
  | EList ys ->
      List.map begin function
        | EString s -> s
        | _ -> fail "%s must be a list of strings" what
      end ys
  | _ -> fail "%s must be a list of strings" what

let read_config_file filename =
  let config = Stml_utils.parse_config_file filename in
  let rec process = function
    | ("mirror", x)::xs ->
        Stml_clflags.mirror := check_string "mirror" x;
        process xs
    | ("areas", x)::xs ->
        Stml_clflags.areas := check_string_list "areas" x;
        process xs
    | ("architectures", x)::xs ->
        Stml_clflags.architectures := check_string_list "architectures" x;
        process xs
    | ("suite", x)::xs ->
        Stml_clflags.suite := check_string "suite" x;
        process xs
    | x::xs ->
        x::(process xs)
    | [] -> []
  in process config

let rec parse_common_args = function
  | ("--dry-run" | "-n")::xs ->
      Stml_clflags.dry_run := true;
      parse_common_args xs
  | ("--quiet" | "-q")::xs ->
      Stml_clflags.quiet := true;
      parse_common_args xs
  | ("--verbose" | "-v")::xs ->
      Stml_clflags.verbose := true;
      parse_common_args xs
  | "--mirror"::x::xs ->
      Stml_clflags.mirror := x;
      parse_common_args xs
  | "--areas"::x::xs ->
      Stml_clflags.areas := Stml_core.simple_split ',' x;
      parse_common_args xs
  | "--archs"::x::xs ->
      Stml_clflags.architectures := Stml_core.simple_split ',' x;
      parse_common_args xs
  | "--suite"::x::xs ->
      Stml_clflags.suite := x;
      parse_common_args xs
  | "--cache-dir"::x::xs ->
      Stml_clflags.cache_dir := x;
      parse_common_args xs
  | ("--config"|"-c")::x::xs ->
      Stml_clflags.config := read_config_file x;
      parse_common_args xs
  | x::xs -> x::(parse_common_args xs)
  | [] -> []

let main () = match Array.to_list Sys.argv with
  | [] ->
      (* we assume Sys.argv.(0) is always here! *)
      assert false
  | cmd::xs ->
      let sc, args =
        try
          let cmd = to_cmd (Filename.basename cmd) in
          get_frontend cmd, xs
        with Error (Unknown_command _) -> match xs with
          | cmd::xs ->
              get_frontend cmd, xs
          | _ ->
              failwith "nothing to do"
      in
      wrap (fun () -> sc.main args)