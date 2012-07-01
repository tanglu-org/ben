(**************************************************************************)
(*  Copyright © 2009-2012 Stéphane Glondu <steph@glondu.net>              *)
(*            © 2010-2012 Mehdi Dogguy <mehdi@dogguy.org>                 *)
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

(** Lexing rules *)

val token : Lexing.lexbuf -> Benl_parser.token
(** Tokenizer for [Benl_parser]. *)

val stanza_fold :
  ((string * string) list -> 'a -> 'a) -> Lexing.lexbuf -> 'a -> 'a
(** [stanza_fold f lexbuf accu] iterates [f] over all stanzas of
    [lexbuf], using [accu] as accumulator. *)
