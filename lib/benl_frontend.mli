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

type frontend = {
  name : string;
  main : unit -> unit;
  anon_fun: string -> unit;
  help : (Benl_arg.key * Benl_arg.spec * Benl_arg.doc) list
}

val spec : (Benl_arg.key * Benl_arg.spec * Benl_arg.doc) list ref

val register_frontend : frontend -> unit
val get_frontend : string -> frontend
val get_selected_frontend : unit -> frontend
val set_selected_frontend : frontend -> unit
val available_frontends : unit -> string list
val check_string : string -> Benl_types.expr -> string
val check_string_list : string -> Benl_types.expr -> string list
val read_config_file : string -> Benl_types.config
