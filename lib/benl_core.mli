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

(** Utility functions and modules.

    This module contains handy functions (or modules) not specific to Ben.
*)

module StringSet : sig
  include Set.S with type elt = string
  val from_list : string list -> t
end

module StringMap : sig
  include Map.S with type key = string
  val from_list : (key * int) list -> int t
end

module IntMap : sig
  include Map.S with type key = int
end

val with_in_channel : in_channel -> (in_channel -> 'a) -> 'a
(** Run the function on the in_channel, taking care of exceptions. *)

val with_in_file : string -> (in_channel -> 'a) -> 'a
(** Run the function on the given file, taking care of exceptions. *)

val with_out_file : string -> (out_channel -> 'a) -> 'a
(** Run the function on the given file, taking care of exceptions .*)

val escape_for_shell : string -> string
(** Quote a string for use with shell. For example, ["a"] is quoted
    into ["'a'"]. *)

val get_rfc2822_date : unit -> string
(** Get the current date in RFC-2822 format. *)

val list_iteri : (int -> 'a -> 'b) -> 'a list -> unit
(** Same as [List.iter], but calls the function with the index of the
    current element. *)

val list_rev_mapi : (int -> 'a -> 'b) -> 'a list -> 'b list
(** Same as [List.rev_map], but calls the function with the index of
    the current element. *)

val simple_split : char -> string -> string list
(** [simple_split sep s] splits [s] using [sep] as delimiter. *)

val capitalize : ?sep:char -> string -> string
(** [capitalize ?sep s] capitalizes a field name. *)

val starts_with : string -> string -> bool
(** [starts_with s prefix] returns [true] iff [s] starts with
    [prefix]. *)

val ends_with : string -> string -> bool
(** [ends_with s suffix] returns [true] iff [s] ends with [suffix]. *)
