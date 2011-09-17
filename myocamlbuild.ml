(**************************************************************************)
(*  Copyright © 2009-2010 Stéphane Glondu <steph@glondu.net>              *)
(*            © 2010 Mehdi Dogguy <mehdi@dogguy.org>                      *)
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

let name = "ben"
let packages = ["unix"; "pcre"; "ocamlgraph"; "ocsigen_xhtml"; "fileutils"; "threads"; "postgresql"]

exception Require_findlib
exception Missing_findlib_package of string
exception Subprocess_died_unexpectedly of Unix.process_status

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
let main_executable = sprintf "bin/%s.%s" name best

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
        Pathname.define_context "lib/benlib" ["lib"];
        Pathname.define_context "frontends" ["lib"];
        Pathname.define_context "bin" ["lib"; "frontends"];
        flag ["ocaml"; "link"; "program"] & A"-linkpkg";
        List.iter
          (fun pkg ->
             let flag x = flag (x::["ocaml"]) & S[A"-package"; A pkg] in
             List.iter flag ["ocamldep"; "compile"; "link"; "doc"])
          packages;

        (* why isn't this done by default? *)
        flag ["library"; "link"; "thread"] (A"-thread");

        (* C stubs *)
        flag ["link"; "library"; "ocaml"; "byte"; "use_libbenl"]
          (S[A"-dllib"; A"-lbenl"; A"-cclib"; A"-lbenl"]);
        flag ["link"; "library"; "ocaml"; "native"; "use_libbenl"]
          (S[A"-cclib"; A"-lbenl"]);
        flag ["link"; "library"; "ocaml"; "native"; "use_libbenl"]
          (S[A"-cclib"; A"-lbenl"]);
        flag ["link"; "program"; "ocaml"; "byte"; "use_libbenl"]
          (S[A"-dllib"; A"-lbenl"]);
        dep  ["link"; "ocaml"; "use_libbenl"] ["lib/libbenl.a"];

        (* rule for the main executable that will link all frontends  *)
        rule "bin/ben.ml" ~deps:["bin/ben.mlp"] ~prod:"bin/ben.ml" begin
          let prefix = name^"_" in
          let p = String.length prefix in
          fun _ _ ->
            let files = Array.to_list (Sys.readdir "../frontends") in
            let static = List.filter
              (fun x ->
                 let n = String.length x in
                 n > p+5 &&
                   String.sub x 0 p = prefix &&
                     String.sub x (n-3) 3 = ".ml")
              files
            in
            let static = List.map
              (fun x -> (Filename.chop_suffix x ".ml")^".frontend")
              static
            in
            let static =
              String.concat "; " (List.map String.capitalize static)
            in
            Cmd
              (S [A"sed";
                  A"-e"; A (sprintf "s/@STATIC_FRONTENDS@/%s/" static);
                  P"bin/ben.mlp"; Sh">"; P"bin/ben.ml"])
        end;

        (* rule for dependency graph *)
        rule "modules.dot" ~deps:[main_executable] ~prod:"modules.dot" begin
          fun _ _ ->
            let ic = Unix.open_process_in "find -name '*.ml.depends'" in
            let buf = Buffer.create 1024 in
            bprintf buf "digraph build_graph {\n";
            let rec loop accu =
              match (try Some (input_line ic) with End_of_file -> None) with
                | None -> accu
                | Some filename ->
                    let node =
                      let n = String.length filename in
                      let j = n - String.length ".ml.depends" in
                      let i =
                        try String.rindex_from filename j '/'
                        with Not_found -> 0
                      in
                      String.capitalize (String.sub filename (i+1) (j-i-1))
                    in
                    let deps = List.tl (string_list_of_file filename) in
                    loop ((node, deps)::accu)
            in
            let deps_assoc = loop [] in
            let module S = Set.Make(String) in
            let mynodes = List.fold_left
              (fun accu (x, _) -> S.add x accu) S.empty deps_assoc
            in
            let () = List.iter
              (fun (x, deps) -> List.iter
                 (fun dep ->
                    if S.mem dep mynodes then
                      bprintf buf "  \"%s\" -> \"%s\";\n" x dep)
                 deps)
              deps_assoc
            in
            bprintf buf "}\n";
            begin match Unix.close_process_in ic with
              | Unix.WEXITED 0 -> ()
              | e -> raise (Subprocess_died_unexpectedly e)
            end;
            Echo ([Buffer.contents buf], "modules.dot")
        end

    | _ -> ()
  end
