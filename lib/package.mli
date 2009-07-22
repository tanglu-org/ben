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

type 'a name
type 'a t

val of_assoc : (string * string) list -> 'a t
val get : string -> 'a t -> string
val print : 'a t -> unit
val name_of_string : string -> 'a name

module Set : sig
  type 'a t
  val empty : 'a t
  val add : 'a name -> 'a t -> 'a t
  val mem : 'a name -> 'a t -> bool
  val exists : ('a name -> bool) -> 'a t -> bool
  val iter : ('a name -> unit) -> 'a t -> unit
  val cardinal : 'a t -> int
  val elements : 'a t -> 'a name list
end

module Map : sig
  type ('a, 'b) t
  val empty : ('a, 'b) t
  val add : 'a name -> 'b -> ('a, 'b) t -> ('a, 'b) t
  val find : 'a name -> ('a, 'b) t -> 'b
  val iter : ('a name -> 'b -> unit) -> ('a, 'b) t -> unit
  val map : ('a name -> 'b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
  val fold : ('a name -> 'b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c
end

val build_depends : [`source] t -> [`binary] name list
