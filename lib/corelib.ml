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

let with_in_file file f =
  let chan = open_in_bin file in
  try
    let res = f chan in
    close_in chan; res
  with e -> close_in chan; raise e

let with_out_file file f =
  let chan = open_out_bin file in
  try
    let res = f chan in
    close_out chan; res
  with e -> close_out chan; raise e

let escape_for_shell str =
  let buf = Buffer.create (2 * String.length str) in
  Buffer.add_char buf '\'';
  String.iter
    (function
       | '\'' -> Buffer.add_string buf "'\\''"
       | c -> Buffer.add_char buf c)
    str;
  Buffer.add_char buf '\'';
  Buffer.contents buf
