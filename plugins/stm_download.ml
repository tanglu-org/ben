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
open Stml_core
open Stml_base
open Stml_error

let p = Stml_clflags.progress
let ( / ) = Filename.concat

let download_sources () =
  if !Stml_clflags.areas = [] then raise Nothing_to_download;
  let wquiet = if !Stml_clflags.verbose then "" else "-q" in
  let dst = !Stml_clflags.cache_dir/"Sources" in
  let tmp = Filename.temp_file "Sources." "" in
  let commands =
    List.map
      (fun area ->
         let url = sprintf "%s/dists/%s/%s/source/Sources.bz2"
           !Stml_clflags.mirror !Stml_clflags.suite area
         in
         if !Stml_clflags.dry_run then p "Would download %s\n" url;
         let cmd = sprintf "{ wget %s -O- %s | bzcat >> %s; }"
           wquiet (escape_for_shell url) tmp
         in cmd)
      !Stml_clflags.areas
  in
  let cmd = sprintf "%s && mv %s %s"
    (String.concat " && " commands) tmp dst
  in
  if not !Stml_clflags.dry_run then begin
    if not !Stml_clflags.verbose then p "Downloading Sources...";
    let r = Sys.command cmd in
    if not !Stml_clflags.verbose then p "\n";
    if r <> 0 then
      raise (Wget_error r)
    else
      ignore (Sys.command (sprintf "rm -f %s" tmp))
  end;;

let download_binaries arch =
  if !Stml_clflags.areas = [] then raise Nothing_to_download;
  let wquiet = if !Stml_clflags.verbose then "" else "-q" in
  let dst = !Stml_clflags.cache_dir/"Packages."^arch in
  let tmp = Filename.temp_file ("Packages.") "" in
  let commands =
    List.map
      (fun area ->
         let url = sprintf "%s/dists/%s/%s/binary-%s/Packages.bz2"
           !Stml_clflags.mirror !Stml_clflags.suite area arch
         in
         if !Stml_clflags.dry_run then p "Would download %s\n" url;
         let cmd = sprintf "{ wget %s -O- %s | bzcat >> %s; }"
           wquiet (escape_for_shell url) tmp
         in
         cmd)
      !Stml_clflags.areas
  in
  let cmd = sprintf "%s && mv %s %s"
    (String.concat " && " commands) tmp dst
  in
  if not !Stml_clflags.dry_run then begin
    if not !Stml_clflags.verbose then p "Downloading Packages/%s..." arch;
    let r = Sys.command cmd in
    p "\n";
    if r <> 0 then
      raise (Wget_error r)
    else
      ignore (Sys.command (sprintf "rm -f %s" tmp))
  end;;

let download_all () =
  download_sources ();
  List.iter download_binaries !Stml_clflags.architectures;;

let main args =
  ignore (Stml_plugin.parse_common_args args);
  download_all ()

let subcommand = {
  Stml_plugin.name = "download";
  Stml_plugin.main = main;
}
