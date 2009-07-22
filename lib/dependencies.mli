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

val get_dep_graph :
  ([`source], [`source] Package.t) Package.Map.t ->
  ([`binary], [`source] Package.name) Package.Map.t ->
  ([`source], [`source] Package.Set.t) Package.Map.t

val invert_dep_graph :
  ([`source], [`source] Package.Set.t) Package.Map.t ->
  ([`source], [`source] Package.Set.t) Package.Map.t

val topo_split :
  ([`source], [`source] Package.Set.t) Package.Map.t ->
  [`source] Package.name list list
