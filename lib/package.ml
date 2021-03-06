(**************************************************************************)
(*  Copyright © 2009-2013 Stéphane Glondu <steph@glondu.net>              *)
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
open Benl_core

type 'a t = string StringMap.t

module Name = struct
  type 'a t = string
  let of_string x = x
  let to_string x = x
end

let filter_print keep outc p =
  StringMap.iter
    (fun f v ->
      if keep = [] || List.mem (String.lowercase f) keep then
        fprintf outc "%s: %s\n" (Benl_core.capitalize f) v)
    p;
  fprintf outc "\n"

let print = filter_print []

let get = StringMap.find
let has = StringMap.mem

let add k v pkg = StringMap.add k v pkg

module Set = struct
  module S = Set.Make(String)
  type 'a t = S.t
  let is_empty = S.is_empty
  let empty = S.empty
  let add = S.add
  let mem = S.mem
  let from_list =
    List.fold_left
      (fun set elt -> add elt set)
      empty
  let exists = S.exists
  let iter = S.iter
  let cardinal = S.cardinal
  let elements = S.elements
  let fold = S.fold
  let filter = S.filter
end

let rex = Re_pcre.regexp "^(\\S+)(?: \\((\\S+)\\))?$"

let of_assoc sort x =
  match sort with
    | `binary ->
        let source, version =
          try
            let name = get "source" x in
            let r = Re_pcre.exec ~rex name in
            let name = Re_pcre.get_substring r 1 in
            let version =
              try
                Re_pcre.get_substring r 2
              with Not_found ->
                get "version" x
            in
            name, version
          with Not_found ->
            get "package" x, get "version" x
        in
        StringMap.add "source" source
          (StringMap.add "source-version" version
             (StringMap.remove "source" x)
          )
    | `source -> x

module Map = struct
  module M = Map.Make(String)
  type ('a, 'b) t = 'b M.t
  let empty = M.empty
  let is_empty = M.is_empty
  let add = M.add
  let iter = M.iter
  let find = M.find
  let mapi = M.mapi
  let fold = M.fold
  let bindings = M.bindings
  let mem = M.mem

  let update_default default f key t =
    let previous = try find key t with Not_found -> default in
    add key (f previous) t
end

let get_and_split =
  let rex = Re_pcre.regexp "(?:[, |]|\\([^)]+\\))+" in
  fun field x ->
    try
      let deps = get field x in
      Re_pcre.split ~rex deps
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
  let rex = Re_pcre.regexp "^\\s*(\\S+)\\s*(\\(([<>=]+)\\s*([^)]+)\\))?\\s*(\\[\\s*([^\\]]+)\\s*\\])?\\s*$" in
  fun x ->
    try
      let r = Re_pcre.exec ~rex x in
      let dep =
        try
          let cmp = match Re_pcre.get_substring r 3 with
            | "<=" -> Le
            | "<<" -> Lt
            | ">=" -> Ge
            | ">>" -> Gt
            | "=" -> Eq
            | "<" -> Lt
            | ">" -> Gt
            | x -> ksprintf failwith "invalid comparison operator: %s" x
          in
          Some (cmp, Re_pcre.get_substring r 4)
        with Not_found ->
          None
      in {
        dep_name = Re_pcre.get_substring r 1;
        dep_version = dep;
      }
    with Not_found ->
      ksprintf failwith "unable to parse: %s" x

let dependencies =
  let rex = Re_pcre.regexp "(?:\\s*[,|]\\s*)+" in
  fun field x ->
    try
      let deps = get field x in
      let deps = Re_pcre.split ~rex deps in
      List.map split_name_and_version deps
    with Not_found ->
      []
