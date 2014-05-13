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

let reset () =
  let () = architectures := !Benl_base.debian_architectures in
  let () = suite := "unstable" in
  let () = areas := ["main"; "contrib"; "non-free"] in
  ()

let config : Benl_types.config ref = ref StringMap.empty
let get_config key =
  try StringMap.find key !config
  with Not_found -> raise (Missing_configuration_item key)

let get_cache_file ?(name = !cache_file) () =
  if Sys.file_exists name
  then name
  else let filecache = Filename.concat !cache_dir name in
       if Sys.file_exists filecache
       then filecache
       else name (* Let the system generate an error *)

let progress fmt =
  if !quiet then
    Printf.ifprintf stderr fmt
  else
    Printf.fprintf stderr (fmt^^"%!")
