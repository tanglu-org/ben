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

open Benl_error
open Template

let template : Template.t option ref = ref None
let path = "/usr/share/ben/templates"

let (//) = Filename.concat

let register_template name page =
  template := Some { name; page }

let load_template name =
  let file = path // name // (Printf.sprintf "%s.cma" name) in
  let file = Dynlink.adapt_filename file in
  try
    Dynlink.loadfile file
  with Dynlink.Error e ->
    Benl_error.raise (Dynlink_error e)

let get_registered_template () =
  match !template with
  | Some t -> t
  | None ->
      let name = "debian" in
      let () = load_template name in begin
      match !template with
      | Some t -> t
      | None -> Benl_error.raise (Benl_error.Template_not_found name)
      end
