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
open Benl_types

type 'a t = (string * string) list

module Name = struct
  type 'a t = string
  let of_string x = x
  let to_string x = x
end

let print p =
  List.iter
    (fun (f, v) -> printf "%s: %s\n" (String.capitalize f) v)
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
  let fold = S.fold
end

let rex = Pcre.regexp "^(\\S+)(?: \\((\\S+)\\))?$"

let of_assoc sort ~debcheck_data x =
  match sort with
    | `binary ->
        let source, version =
          try
            let name = get "source" x in
            let r = Pcre.exec ~rex name in
            let name = Pcre.get_substring r 1 in
            let version =
              try
                Pcre.get_substring r 2
              with Not_found ->
                get "version" x
            in
            name, version
          with Not_found ->
            get "package" x, get "version" x
        in
        let x =
          ("source", source) ::
          ("source-version", version) ::
          (List.remove_assoc "source" x)
        in
        begin match debcheck_data with
          | None -> x
          | Some data ->
              let name = get "package" x in
              try
                if Set.mem name data then
                  ("edos-debcheck", "uninstallable")::x
                else x
              with Not_found -> x
        end
    | `source -> x

module BinaryIndex = struct
  type t = [`binary] Name.t * string
  let compare = Pervasives.compare
end
module BinaryMap = Map.Make(BinaryIndex)

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

let get_and_split =
  let rex = Pcre.regexp "(?:[, |]|\\([^)]+\\))+" in
  fun field x ->
    try
      let deps = get field x in
      Pcre.split ~rex deps
    with Not_found -> []

let build_depends x =
  get_and_split "build-depends-indep" x @ get_and_split "build-depends" x

let binaries x =
  get_and_split "binary" x

type dependency = {
  dep_name : string;
  dep_version : (comparison * string) option;
}

let split_name_and_version =
  let rex = Pcre.regexp "^\\s*(\\S+)\\s*(\\((\\S+)\\s*([^)]+)\\))?\\s*$" in
  fun x ->
    try
      let r = Pcre.exec ~rex x in
      let dep =
        try
          let cmp = match Pcre.get_substring r 3 with
            | "<=" -> Le
            | "<<" -> Lt
            | ">=" -> Ge
            | ">>" -> Gt
            | "=" -> Eq
            | "<" -> Lt
            | ">" -> Gt
            | x -> ksprintf failwith "invalid comparison operator: %s" x
          in
          Some (cmp, Pcre.get_substring r 4)
        with Not_found ->
          None
      in {
        dep_name = Pcre.get_substring r 1;
        dep_version = dep;
      }
    with Not_found ->
      ksprintf failwith "unable to parse: %s" x

let dependencies =
  let rex = Pcre.regexp "(?:\\s*[,|]\\s*)+" in
  fun field x ->
    try
      let deps = get field x in
      let deps = Pcre.split ~rex deps in
      List.map split_name_and_version deps
    with Not_found ->
      []
