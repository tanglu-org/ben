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

open Stml_core
open Stml_base
open Stml_error

module type MARSHALLABLE = sig
  type t
  val magic_number : string
end

module Make (I : MARSHALLABLE) = struct

  let load filename =
    with_in_file filename begin
      fun ic ->
        let n = String.length I.magic_number in
        let buf = String.create n in
        really_input ic buf 0 n;
        if buf = I.magic_number then begin
          (input_value ic : I.t)
        end else begin
          raise (Bad_marshalled_data filename)
        end
    end

  let dump filename (data : I.t) =
    with_out_file filename begin
      fun ic ->
        output_string ic I.magic_number;
        output_value ic data
    end

end
