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
  Xhtml_types.h2_content Xhtml.M.elt list -> (* Subtitle *)
  [ `Link | `Meta | `Object | `Script | `Style ] Xhtml.M.elt list -> (* Extra Headers *)
  Xhtml_types.div_content Xhtml.M.elt list -> (* Body *)
  Xhtml_types.div_content Xhtml.M.elt list -> (* Footer *)
  Xhtml.M.html

type changelog_url_t  = (string -> string -> string -> string, unit, string) format
type buildd_url_t     = (string -> string -> string, unit, string) format
type buildds_url_t    = (string -> string, unit, string) format option
type pts_url_t        = (string -> string, unit, string) format
type msg_id_service_t = (string -> string, unit, string) format
type bugs_t           = (string -> string, unit, string) format
type critical_bugs_t  = (string -> string, unit, string) format option

type t = {
  name : string ;
  page : page_t;
  intro : Xhtml_types.div_content Xhtml.M.elt list;
  changelog : changelog_url_t;
  buildd : buildd_url_t;
  buildds : buildds_url_t;
  pts : pts_url_t;
  msg_id : msg_id_service_t;
  bugs : bugs_t;
  critical_bugs : critical_bugs_t;
}
