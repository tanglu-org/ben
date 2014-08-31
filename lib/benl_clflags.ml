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

open Benl_error
open Benl_core

let get_env_default var default =
  try Sys.getenv var with Not_found -> default

let show_version = ref false
let dry_run = ref false
let verbose = ref false
let architectures = ref !Benl_base.debian_architectures
let cache_dir = ref (get_env_default "BEN_CACHE_DIR" (Sys.getcwd ()))
let cache_file = ref "ben.cache"
let use_cache = ref false
let use_benrc = ref true
let media_dir = ref (get_env_default "BEN_MEDIA_DIR" "/usr/share/ben/media")
let mirror_binaries = ref "http://ftp.fr.debian.org/debian"
let mirror_sources = ref "http://ftp.fr.debian.org/debian"
let mirror = mirror_binaries
let suite = ref "unstable"
let areas = ref ["main"; "contrib"; "non-free"]
let preferred_compression_format = ref Benl_compression.default
let quiet = ref false
let update = ref false

let get_config config key =
  try StringMap.find key config
  with Not_found -> Benl_error.raise (Benl_error.Missing_configuration_item key)

let set_cache_file name =
  let basename = Filename.basename name in
  let dirname = Filename.dirname name in
  cache_dir := dirname;
  cache_file := basename

let get_cache_file () =
  Filename.concat !cache_dir !cache_file
  (* Let the system generate an error if the file is missing *)

let progress fmt =
  if !quiet then
    Printf.ifprintf stderr fmt
  else
    Printf.fprintf stderr (fmt^^"%!")
