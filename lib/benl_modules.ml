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

module PAIndex = struct
  type t = [`binary] Package.Name.t * string
  let compare = Pervasives.compare
end
module PAMap = struct
  include Map.Make(PAIndex)
  let fusion m1 m2 =
    let smerge _ v1 v2 = match v1, v2 with
      | Some v1, Some v2 -> Some v1
      | Some v1, None    -> Some v1
      | None   , Some v2 -> Some v2
      | _                -> None
    in
    merge smerge m1 m2
end

module Marshallable = struct
  let magic_number = "BENA0902"
  type t = {
    src_map : ([`source], [`source] Package.t) Package.Map.t;
    bin_map : [`binary] Package.t PAMap.t
  }
end
