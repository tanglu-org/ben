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

val parse_control_in_channel :
  ([< `binary | `source] as 'a) ->
  string -> in_channel -> (string -> bool) ->
  ('a Package.Name.t -> 'a Package.t -> 'b -> 'b) ->
  'b -> 'b

val parse_control_file :
  ([< `binary | `source] as 'a) ->
  string -> (string -> bool) ->
  ('a Package.Name.t -> 'a Package.t -> 'b -> 'b) ->
  'b -> 'b

val parse_config_from_in_channel : ?filename:string -> in_channel -> Benl_types.config
val parse_config_file : string -> Benl_types.config

val file_content : string -> string
val dump_to_file : string -> string -> unit
val dump_xhtml_to_file : string -> Xhtml.M.html -> unit
