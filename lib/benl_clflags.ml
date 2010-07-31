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

open Benl_error

let get_env_default var default =
  try Sys.getenv var with Not_found -> default

let dry_run = ref false
let verbose = ref false
let architectures = ref Benl_base.debian_architectures
let cache_dir = ref (get_env_default "BEN_CACHE_DIR" (Sys.getcwd ()))
let mirror = ref "http://ftp.fr.debian.org/debian"
let suite = ref "unstable"
let areas = ref ["main"; "contrib"; "non-free"]
let quiet = ref false

let config : Benl_types.config ref = ref []
let get_config key =
  try List.assoc key !config
  with Not_found -> raise (Missing_configuration_item key)

let progress fmt =
  if !quiet then
    Printf.ifprintf stderr fmt
  else
    Printf.fprintf stderr (fmt^^"%!")
