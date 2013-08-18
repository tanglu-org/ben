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

open Benl_core
open Benl_error
open Benl_types
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

let string_of_string escaping string =
  if escaping then
    let escape = choose_escape string ['"'; '\''] in
    sprintf "%c%s%c" escape string escape
  else
    string

let debian_architectures = ref
  [ "amd64"; "armel";
    "i386"; "ia64";
    "kfreebsd-amd64"; "kfreebsd-i386";
    "mips"; "mipsel"; "powerpc"; "s390"; "sparc" ]

let ignored_architectures : string list ref = ref []

let debian_ports_architectures =
  [ "armhf"; "avr32"; "m68k"; "powerpcspe";
    "sh4"; "sparc64" ]

type status = Unknown | Up_to_date | Outdated

let string_of_status = function
  | Unknown -> " "
  | Up_to_date -> "✔"
  | Outdated -> "✘"

let class_of_status = function
  | Unknown -> "unknown"
  | Up_to_date -> "good"
  | Outdated -> "bad"

module Version : sig

  type t = string
  val compare : t -> t -> int

end = struct

  type t = string

  external verrevcmp : string -> string -> int = "caml_verrevcmp"

  let decomp =
    let rex = Pcre.regexp "^(?:(\\d+):)?(?:([^\\s-]+)|(\\S+)-([^\\s-]+))$" in
    fun x ->
      try
        let r = Pcre.exec ~rex x in
        let epoch =
          try int_of_string (Pcre.get_substring r 1)
          with Not_found -> 0
        in
        let upstream =
          try Pcre.get_substring r 2
          with Not_found -> Pcre.get_substring r 3
        in
        let debian =
          try Pcre.get_substring r 4
          with Not_found -> "0"
        in
        (epoch, upstream, debian)
      with Not_found ->
        ksprintf invalid_arg "invalid version number: %s" x

  let compare x y =
    let (x1, x2, x3) = decomp x and (y1, y2, y3) = decomp y in
    let (>>=) x f = if x = 0 then f () else x in
    let cmp x y () = verrevcmp x y in
    x1 - y1 >>= cmp x2 y2 >>= cmp x3 y3

end

let version_compare cmp x y =
  let d = Version.compare x y in
  match cmp with
    | Eq -> d = 0
    | Ge -> d >= 0
    | Gt -> d > 0
    | Le -> d <= 0
    | Lt -> d < 0
