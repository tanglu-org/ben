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
open Benl_core
open Benl_base
open Benl_error

let p = Benl_clflags.progress
let ( / ) = Filename.concat

let download_sources () =
  if !Benl_clflags.areas = [] then raise Nothing_to_download;
  let wquiet = if !Benl_clflags.verbose then "" else "-s" in
  let dst = !Benl_clflags.cache_dir/"Sources" in
  let tmp = Filename.temp_file "Sources." "" in
  let commands =
    List.map
      (fun area ->
         let url = sprintf "%s/dists/%s/%s/source/Sources.bz2"
           !Benl_clflags.mirror !Benl_clflags.suite area
         in
         if !Benl_clflags.dry_run then p "Would download %s\n" url;
         let cmd = sprintf "{ curl %s %s | bzcat >> %s; }"
           wquiet (escape_for_shell url) tmp
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
      ignore (Sys.command (sprintf "rm -f %s" tmp))
  end;;

let download_binaries arch =
  if !Benl_clflags.areas = [] then raise Nothing_to_download;
  let wquiet = if !Benl_clflags.verbose then "" else "-s" in
  let dst = !Benl_clflags.cache_dir/"Packages."^arch in
  let tmp = Filename.temp_file ("Packages.") "" in
  let commands =
    List.map
      (fun area ->
         let url = sprintf "%s/dists/%s/%s/binary-%s/Packages.bz2"
           !Benl_clflags.mirror !Benl_clflags.suite area arch
         in
         if !Benl_clflags.dry_run then p "Would download %s\n" url;
         let cmd = sprintf "{ curl %s %s | bzcat >> %s; }"
           wquiet (escape_for_shell url) tmp
         in
         cmd)
      !Benl_clflags.areas
  in
  let cmd = sprintf "%s && mv %s %s"
    (String.concat " && " commands) tmp dst
  in
  if not !Benl_clflags.dry_run then begin
    if not !Benl_clflags.verbose then p "Downloading Packages.%s..." arch;
    let r = Sys.command cmd in
    p "\n";
    if r <> 0 then
      raise (Curl_error r)
    else
      ignore (Sys.command (sprintf "rm -f %s" tmp))
  end;;

let download_all () =
  download_sources ();
  List.iter download_binaries !Benl_clflags.architectures;;

let main args =
  ignore (Benl_frontend.parse_common_args args);
  download_all ()

let frontend = {
  Benl_frontend.name = "download";
  Benl_frontend.main = main;
  Benl_frontend.help = fun () -> ();
}
