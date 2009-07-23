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

open Corelib
open Stmerr
open Types
open Printf

module Fields = Set.Make(String)

let choose_escape str =
  let rec loop = function
    | c::cs -> if String.contains str c then loop cs else c
    | _ -> Pervasives.raise Not_found
  in loop ['/'; '@'; ','; '%']

let string_of_regexp (regexp, _) =
  let escape = choose_escape regexp in
  if escape = '/' then
    sprintf "/%s/" regexp
  else
    sprintf "@%c%s%c" escape regexp escape

let core_fields =
  List.fold_right Fields.add
    ["package"; "source"; "binary"; "provides"; "version"; "architecture"; "build-depends"]
    Fields.empty

let progress fmt =
  if !Clflags.quiet_mode then
    ifprintf stderr fmt
  else
    fprintf stderr (fmt^^"%!")

let download_sources mirror suite sections dest =
  if sections = [] then raise Nothing_to_download;
  let tmp = Filename.temp_file "Sources." "" in
  let commands =
    List.map
      (fun section ->
         let url = sprintf "%s/dists/%s/%s/source/Sources.bz2" mirror suite section in
         let cmd = sprintf "{ wget -q -O- %s | bzcat >> %s; }" (escape_for_shell url) tmp in
         cmd)
      sections
  in
  let cmd = sprintf "%s && mv %s %s" (String.concat " && " commands) tmp dest in
  progress "Downloading Sources...";
  let r = Sys.command cmd in
  progress "\n";
  if r <> 0 then
    raise (Wget_error r)
  else
    ignore (Sys.command (sprintf "rm -f %s" tmp));;

let download_packages mirror suite sections arch dest =
  if sections = [] then raise Nothing_to_download;
  let tmp = Filename.temp_file ("Packages.") "" in
  let commands =
    List.map
      (fun section ->
         let url = sprintf "%s/dists/%s/%s/binary-%s/Packages.bz2" mirror suite section arch in
         let cmd = sprintf "{ wget -q -O- %s | bzcat >> %s; }" (escape_for_shell url) tmp in
         cmd)
      sections
  in
  let cmd = sprintf "%s && mv %s %s" (String.concat " && " commands) tmp dest in
  progress "Downloading Packages/%s..." arch;
  let r = Sys.command cmd in
  progress "\n";
  if r <> 0 then
    raise (Wget_error r)
  else
    ignore (Sys.command (sprintf "rm -f %s" tmp));;

let download_all () =
  let (/) = Filename.concat in
  download_sources
    !Clflags.mirror
    !Clflags.suite
    !Clflags.sections
    (!Clflags.cache_dir/"Sources");
  List.iter
    (fun arch -> download_packages
       !Clflags.mirror
       !Clflags.suite
       !Clflags.sections
       arch
       (!Clflags.cache_dir/("Packages."^arch)))
    !Clflags.architectures;;

let wrap f =
  try f ()
  with Error e -> eprintf "stm error: %s\n" (string_of_exn e)

type status = Unknown | Up_to_date | Outdated

let string_of_status = function
  | Unknown -> " "
  | Up_to_date -> "✔"
  | Outdated -> "✘"

let class_of_status = function
  | Unknown -> "unknown"
  | Up_to_date -> "good"
  | Outdated -> "bad"
