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

type 'a t = (string * string) list

module Name = struct
  type 'a t = string
  let of_string x = x
  let to_string x = x
end

let of_assoc x = x

let print p =
  List.iter
    (fun (f, v) -> printf "%s: %s\n" f v)
    p;
  print_newline ()

let get = List.assoc

module Set = struct
  module S = Set.Make(String)
  type 'a t = S.t
  let empty = S.empty
  let add = S.add
  let mem = S.mem
  let exists = S.exists
  let iter = S.iter
  let cardinal = S.cardinal
  let elements = S.elements
end

module Map = struct
  module M = Map.Make(String)
  type ('a, 'b) t = 'b M.t
  let empty = M.empty
  let add = M.add
  let iter = M.iter
  let find = M.find
  let mapi = M.mapi
  let fold = M.fold

  let update_default default f key t =
    let previous = try find key t with Not_found -> default in
    add key (f previous) t
end

let build_depends =
  let rex = Pcre.regexp "([, |]|\\([^)]+\\))+" in
  fun x ->
    let deps = get "build-depends" x in
    Pcre.split ~rex deps
