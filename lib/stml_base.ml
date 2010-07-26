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

open Stml_core
open Stml_error
open Stml_types
open Printf

module Fields = Set.Make(String)

let choose_escape str xs =
  let rec loop = function
    | c::cs -> if String.contains str c then loop cs else c
    | _ -> Pervasives.raise Not_found
  in loop xs

let string_of_regexp (regexp, _) =
  let escape = choose_escape regexp ['/'; '@'; ','; '%'] in
  if escape = '/' then
    sprintf "/%s/" regexp
  else
    sprintf "@%c%s%c" escape regexp escape

let string_of_cmp = function
  | Le -> "<="
  | Lt -> "<<"
  | Eq -> "="
  | Gt -> ">>"
  | Ge -> ">="

let string_of_string string =
  let escape = choose_escape string ['"'; '\''] in
  sprintf "%c%s%c" escape string escape

let core_fields =
  List.fold_right Fields.add
    ["package"; "source"; "binary"; "provides"; "version"; "architecture"; "build-depends"]
    Fields.empty

let debian_architectures =
  [ "alpha"; "amd64"; "armel";
    "hppa"; "i386"; "ia64";
    "kfreebsd-amd64"; "kfreebsd-i386";
    "mips"; "mipsel"; "powerpc"; "s390"; "sparc" ]

type status = Unknown | Up_to_date | Outdated

let string_of_status = function
  | Unknown -> " "
  | Up_to_date -> "✔"
  | Outdated -> "✘"

let class_of_status = function
  | Unknown -> "unknown"
  | Up_to_date -> "good"
  | Outdated -> "bad"

let () = ignore (Perl.eval "use Dpkg::Version")

let version_compare x y =
  Perl.int_of_sv
    (Perl.call
       ~fn:"Dpkg::Version::version_compare"
       [Perl.sv_of_string x; Perl.sv_of_string y])

let version_compare_cmp cmp x y =
  let d = version_compare x y in
  match cmp with
    | Eq -> d = 0
    | Ge -> d >= 0
    | Gt -> d > 0
    | Le -> d <= 0
    | Lt -> d < 0
