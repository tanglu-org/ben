(**************************************************************************)
(*  Copyright © 2009-2014 Stéphane Glondu <steph@glondu.net>              *)
(*            © 2010-2014 Mehdi Dogguy <mehdi@dogguy.org>                 *)
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

type t = Gzip | Bz2 | Xz | Plain

let to_string = function
  | Gzip -> "Gzip"
  | Bz2 -> "Bz2"
  | Xz -> "Xz"
  | Plain -> "Plain"

let of_string s = match (String.lowercase s) with
  | "gzip" | "gz" -> Gzip
  | "bz2" -> Bz2
  | "xz" -> Xz
  | "plain" | "no" | "none" -> Plain
  | _ -> raise (Unknown_input_format s)

let default = Gzip

let is_known s =
  try
    ignore (of_string s);
    true
  with _ ->
    false

let is_compressed s =
  try
    (of_string s) <> Plain
  with _ ->
    false

let file_extension filename =
  try
    Some (FilePath.get_extension filename)
  with _ ->
    None

let file_is_compressed filename =
  try
    is_compressed (FilePath.get_extension filename)
  with _ ->
    false

let extension = function
  | Gzip -> ".gz"
  | Bz2 -> ".bz2"
  | Xz -> ".xz"
  | Plain -> ""

let display_tool = function
  | Gzip -> "zcat"
  | Bz2 -> "bzcat"
  | Xz -> "xzcat"
  | Plain -> "cat"
