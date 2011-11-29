(**************************************************************************)
(*  Copyright © 2009-2010 Stéphane Glondu <steph@glondu.net>              *)
(*            © 2010 Mehdi Dogguy <mehdi@dogguy.org>                      *)
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

type frontend = {
  name : string;
  main : string list -> unit;
  help : unit -> unit
}

val register_frontend : frontend -> unit
val get_frontend : string -> frontend
val available_frontends : unit -> string list
val parse_common_args : string list -> string list
val check_string : string -> Benl_types.expr -> string
val check_string_list : string -> Benl_types.expr -> string list
val read_config_file : string -> (string * Benl_types.expr) list
val main : unit -> unit
