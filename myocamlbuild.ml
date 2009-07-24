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

open Printf
open Ocamlbuild_plugin

let name = "stm"
let packages = ["unix"; "pcre"; "ocamlgraph"]

exception Require_findlib
exception Missing_findlib_package of string

let try_exec cmd =
  Sys.command (sprintf "%s >/dev/null 2>&1" cmd) = 0

let require pkg =
  if not (try_exec (sprintf "ocamlfind query %s" pkg)) then
    raise (Missing_findlib_package pkg)

let ocamlfind x = S[A"ocamlfind"; A x]
let has_ocamlopt = try_exec "which ocamlopt"
let best = if has_ocamlopt then "native" else "byte"
let _ = if not (try_exec "ocamlfind printconf") then raise Require_findlib
let _ = List.iter require packages

let _ =
  dispatch begin function

    | Before_options ->
        Options.ocamlc := ocamlfind "ocamlc";
        Options.ocamlopt := ocamlfind "ocamlopt";
        Options.ocamldep := ocamlfind "ocamldep";
        Options.ocamldoc := ocamlfind "ocamldoc";
        Options.use_menhir := true;
        Options.ocaml_yaccflags := ["--explain"]

    | After_rules ->
        Pathname.define_context "plugins" ["lib"];
        Pathname.define_context "bin" ["lib"; "plugins"];
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";
        List.iter
          (fun pkg ->
             let flag x = flag (x::["ocaml"]) & S[A"-package"; A pkg] in
             List.iter flag ["ocamldep"; "compile"; "link"; "doc"])
          packages;

        (* rule for the main executable that will link all plugins  *)
        rule "bin/stm.ml" ~deps:["bin/stm.mlp"] ~prod:"bin/stm.ml" begin
          let prefix = name^"_" in
          let p = String.length prefix in
          fun _ _ ->
            let files = Array.to_list (Sys.readdir "../plugins") in
            let static = List.filter
              (fun x ->
                 let n = String.length x in
                 n > p+5 &&
                   String.sub x 0 p = prefix &&
                     String.sub x (n-3) 3 = ".ml")
              files
            in
            let static = List.map
              (fun x -> (Filename.chop_suffix x ".ml")^".subcommand")
              static
            in
            let static =
              String.concat "; " (List.map String.capitalize static)
            in
            Cmd
              (S [A"sed";
                  A"-e"; A (sprintf "s/@STATIC_PLUGINS@/%s/" static);
                  P"bin/stm.mlp"; Sh">"; P"bin/stm.ml"])
        end

    | _ -> ()
  end
