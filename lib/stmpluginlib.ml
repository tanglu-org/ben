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

open Stmerr

type subcommand = {
  name : string;
  main : string list -> unit
}
let subcommands = ref []

let register_subcommand sc =
  subcommands := (sc.name, sc) :: !subcommands

let get_subcommand x =
  try List.assoc x !subcommands
  with Not_found -> raise (Unknown_command x)

let to_cmd x =
  let n = String.length x in
  if n > 4 && String.sub x 0 4 = "stm-" then
    String.sub x 4 (n-4)
  else x

let main () = match Array.to_list Sys.argv with
  | [] ->
      (* we assume Sys.argv.(0) is always here! *)
      assert false
  | cmd::xs ->
      let sc, args =
        try
          let cmd = to_cmd (Filename.basename cmd) in
          get_subcommand cmd, xs
        with Error (Unknown_command _) -> match xs with
          | cmd::xs ->
              get_subcommand cmd, xs
          | _ ->
              failwith "nothing to do"
      in
      wrap (fun () -> sc.main args)
