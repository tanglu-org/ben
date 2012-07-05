(**************************************************************************)
(*  Copyright © 2012 Mehdi Dogguy <mehdi@debian.org>                      *)
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

type page_t =
  string -> (* Title *)
  [ `Link | `Meta | `Object | `Script | `Style ] Xhtml.M.elt list -> (* Extra Headers *)
  Xhtml_types.div_content Xhtml.M.elt list -> (* Body *)
  Xhtml_types.div_content Xhtml.M.elt list -> (* Footer *)
  Xhtml.M.html

type t = {
  name : string ;
  page : page_t
}
