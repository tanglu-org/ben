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

module StringMap = struct
  include Map.Make(String)
  let from_list =
    List.fold_left
      (fun map (key, value) -> add key value map)
      empty
end

module StringSet = struct
  include Set.Make(String)
  let from_list =
    List.fold_left
      (fun set elt -> add elt set)
      empty
end

module IntMap = Map.Make(struct
  type t = int
  let compare : t -> t -> int = compare
end)

let with_in_channel chan f =
  try
    let res = f chan in
    close_in chan; res
  with e -> close_in chan; raise e

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

let get_rfc2822_date () =
  let chan = Unix.open_process_in "date -R" in
  let r = input_line chan in
  match Unix.close_process_in chan with
    | Unix.WEXITED 0 -> r
    | _ -> failwith "unexpected return of date"

let list_iteri f xs =
  let rec aux i = function
    | [] -> ()
    | x::xs -> f i x; aux (i+1) xs
  in aux 0 xs

let list_rev_mapi f xs =
  let rec aux i accu = function
    | [] -> accu
    | x::xs -> aux (i+1) ((f i x)::accu) xs
  in aux 0 [] xs

let simple_split delim str =
  let n = String.length str in
  let rec aux i accu =
    if i < n then
      let j = try String.index_from str i delim with Not_found -> n in
      aux (j+1) (String.sub str i (j-i) :: accu)
    else List.rev accu
  in aux 0 []

let capitalize ?(sep = '-') s =
  let l = simple_split sep s in
  let l = List.map String.capitalize l in
  String.concat (String.make 1 sep) l

let starts_with str x =
  let n = String.length str and p = String.length x in
  n > p && String.sub str 0 p = x

let ends_with str x =
  let n = String.length str and p = String.length x in
  n > p && String.sub str (n-p) p = x
