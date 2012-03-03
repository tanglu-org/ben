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

open Benl_types

type 'a t

module Name : sig
  type 'a t
  val of_string : string -> 'a t
  val to_string : 'a t -> string
end

val get : string -> 'a t -> string
val add : string -> string -> 'a t -> 'a t
val print : out_channel -> 'a t -> unit

module Set : sig
  type 'a t
  val empty : 'a t
  val add : 'a Name.t -> 'a t -> 'a t
  val mem : 'a Name.t -> 'a t -> bool
  val exists : ('a Name.t -> bool) -> 'a t -> bool
  val iter : ('a Name.t -> unit) -> 'a t -> unit
  val cardinal : 'a t -> int
  val elements : 'a t -> 'a Name.t list
  val fold : ('a Name.t -> 'b -> 'b) -> 'a t -> 'b -> 'b
end

val of_assoc :
  ([< `binary | `source] as 'a) ->
  (string * string) list -> 'a t

module Map : sig
  type ('a, 'b) t
  val empty : ('a, 'b) t
  val add : 'a Name.t -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val find : 'a Name.t -> ('a, 'b) t -> 'b
  val iter : ('a Name.t -> 'b -> unit) -> ('a, 'b) t -> unit
  val mapi : ('a Name.t -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val fold : ('a Name.t -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
  val update_default : 'b -> ('b -> 'b) -> 'a Name.t -> ('a, 'b) t -> ('a, 'b) t
end

val build_depends : [`source] t -> [`binary] Name.t list

type dependency = {
  dep_name : string;
  dep_version : (comparison * string) option;
}

val dependencies : string -> 'a t -> dependency list
