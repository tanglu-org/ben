(**************************************************************************)
(*  Copyright © 2014 Mehdi Dogguy <mehdi@dogguy.org>                      *)
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

let level = ref (Parmap.get_default_ncores ())

let set_level l =
  if l > 0 then begin
    Parmap.set_default_ncores l;
    level := l
  end

let get_level () =
  !level

let map ?(level = !level) f l =
  Parmap.parmap ~ncores:level
    f
    (Parmap.L l)

let iter ?(level = !level) f l =
  Parmap.pariter ~ncores:level
    f
    (Parmap.L l)
