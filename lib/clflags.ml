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

let quiet_mode = ref false

let architectures = ref
  [ "alpha"; "amd64"; "armel";
    "hppa"; "i386"; "ia64";
    "kfreebsd-amd64"; "kfreebsd-i386";
    "mips"; "mipsel"; "powerpc"; "s390"; "sparc" ]

let cache_dir = ref "/home/steph/tmp/mirror"
let mirror = ref "http://ftp.fr.debian.org"
let suite = ref "unstable"
let sections = ref [ "main"; "contrib"; "non-free" ]
