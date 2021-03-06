(**************************************************************************)
(*  Copyright © 2009 Stéphane Glondu <steph@glondu.net>                   *)
(*  Copyright © 2013 Johannes Schauer <j.schauer@email.de>                *)
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
open Benl_core
open Benl_base
open Benl_error
open Benl_data
open Benl_modules
module Marshal = Benl_marshal.Make(Marshallable)
open Marshallable

let p = Benl_clflags.progress
let ( / ) = Filename.concat

let download_sources () =
  if !Benl_clflags.areas = [] then raise Nothing_to_download;
  let wquiet = if !Benl_clflags.verbose then "" else "-s" in
  let dst = !Benl_clflags.cache_dir/"Sources" in
  let tmp = Filename.temp_file "Sources." "" in
  let commands =
    Benl_parallel.map
      (fun area ->
         let url = sprintf "%s/dists/%s/%s/source/Sources%s"
           !Benl_clflags.mirror_sources
           !Benl_clflags.suite
           area
           (Benl_compression.extension !Benl_clflags.preferred_compression_format)
         in
         if !Benl_clflags.dry_run then p "Would download %s\n" url;
         let cmd = sprintf "{ curl %s %s | %s >> %s; }"
           wquiet
           (escape_for_shell url)
           (Benl_compression.display_tool !Benl_clflags.preferred_compression_format)
           tmp
         in cmd)
      !Benl_clflags.areas
  in
  let cmd = sprintf "%s && mv %s %s"
    (String.concat " && " commands) tmp dst
  in
  if not !Benl_clflags.dry_run then begin
    if not !Benl_clflags.verbose then p "Downloading Sources...";
    let r = Sys.command cmd in
    if not !Benl_clflags.verbose then p "\n";
    if r <> 0 then
      raise (Curl_error r)
    else
      FileUtil.rm ~force:FileUtil.Force [tmp]
  end;;

let download_binaries arch =
  if !Benl_clflags.areas = [] then raise Nothing_to_download;
  let wquiet = if !Benl_clflags.verbose then "" else "-s" in
  let dst = !Benl_clflags.cache_dir/"Packages_"^arch in
  let tmp = Filename.temp_file ("Packages.") "" in
  let commands =
    Benl_parallel.map
      (fun area ->
         let url = sprintf "%s/dists/%s/%s/binary-%s/Packages%s"
           !Benl_clflags.mirror_binaries
           !Benl_clflags.suite
           area
           arch
           (Benl_compression.extension !Benl_clflags.preferred_compression_format)
         in
         if !Benl_clflags.dry_run then p "Would download %s\n" url;
         let cmd = sprintf "{ curl %s %s | %s >> %s; }"
           wquiet
           (escape_for_shell url)
           (Benl_compression.display_tool !Benl_clflags.preferred_compression_format)
           tmp
         in
         cmd)
      !Benl_clflags.areas
  in
  let cmd = sprintf "%s && mv %s %s"
    (String.concat " && " commands) tmp dst
  in
  if not !Benl_clflags.dry_run then begin
    if not !Benl_clflags.verbose then p "Downloading Packages_%s..." arch;
    let r = Sys.command cmd in
    p "\n";
    if r <> 0 then
      raise (Curl_error r)
    else
      FileUtil.rm ~force:FileUtil.Force [tmp]
  end;;

let download_all architectures =
  download_sources ();
  Benl_parallel.iter download_binaries architectures;;

let save_cache () =
  if !Benl_clflags.use_cache then begin
    let src_raw = Benl_data.file_origin.get_sources M.empty in
    let bin_raw = Benl_parallel.fold
      Benl_data.file_origin.get_binaries
      PAMap.empty
      !Benl_clflags.architectures
      PAMap.fusion
    in
    let data = { src_map = src_raw; bin_map = bin_raw; } in
    let file = Benl_clflags.get_cache_file () in
    Marshal.dump file data;
  end

let main args =
  download_all !Benl_clflags.architectures;
  save_cache ()

let frontend = {
  Benl_frontend.name = "download";
  Benl_frontend.main = main;
  Benl_frontend.anon_fun = (fun _ -> ());
  Benl_frontend.help = [];
}
