(**************************************************************************)
(*  Copyright © 2009-2013 Stéphane Glondu <steph@glondu.net>              *)
(*            © 2010-2013 Mehdi Dogguy <mehdi@dogguy.org>                 *)
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
open Benl_core
open Benl_base
open Benl_marshal

module M = Package.Map
module S = Package.Set

let use_projectb = ref false
let run_debcheck = ref false

open Benl_modules
module Marshal = Benl_marshal.Make(Marshallable)
open Marshallable

type origin = {
  get_binaries :
    ([ `binary ] as 'a) Package.t PAMap.t -> string -> 'a Package.t PAMap.t;
  get_sources :
    ([ `source ] as 'b, 'b Package.t) M.t -> ('b, 'b Package.t) M.t;
}

let relevant_binary_keys =
  [ "package"; "source"; "version"; "maintainer"; "architecture";
    "provides"; "depends"; "pre-depends"; "replaces";
    "multi-arch";
    "conflicts"; "breaks"; "suggests"; "recommends"; "enhances" ]

let relevant_source_keys =
  [ "package"; "source"; "version"; "maintainer"; "architecture";
    "directory";
    "binary"; "build-depends"; "build-depends-indep" ]

let ( // ) = Filename.concat
let ( !! ) = Lazy.force
let ( !!! ) = Package.Name.to_string

let file_origin =
  let get_binaries accu arch =
    Benl_utils.parse_control_file `binary
      (!Benl_clflags.cache_dir // ("Packages_"^arch))
      (fun x -> List.mem x relevant_binary_keys)
      (fun name pkg accu ->
        try
          let old_pkg = PAMap.find (name, arch) accu in
          let old_ver = Package.get "version" old_pkg in
          let ver = Package.get "version" pkg in
          if Benl_base.Version.compare old_ver ver < 0
          then PAMap.add (name, arch) pkg accu
          else accu
        with _ ->
          PAMap.add (name, arch) pkg accu
      )
      accu
  in
  let get_sources accu =
    Benl_utils.parse_control_file `source
      (!Benl_clflags.cache_dir // "Sources")
      (fun x -> List.mem x relevant_source_keys)
      (fun name pkg accu ->
        try
          let old_pkg = M.find name accu in
          let old_ver = Package.get "version" old_pkg in
          let ver = Package.get "version" pkg in
          if Benl_base.Version.compare old_ver ver < 0
          then M.add name pkg accu
          else accu
        with _ ->
          M.add name pkg accu
      )
      accu
  in
  { get_binaries = get_binaries; get_sources = get_sources }

module Projectb = struct

  module StringMap = Map.Make(String)
  module StringSet = Set.Make(String)
  module IntMap = Map.Make(struct
    type t = int
    let compare : t -> t -> int = compare
  end)

  let mk_origin () =

    let suite = !Benl_clflags.suite in

    (* psql service=projectb must work, e.g. on coccia.debian.org. To make
       it work elsewhere, copy
       coccia.debian.org:/etc/postgresql-common/pg_service.conf to your
       ~/.pg_service.conf and set up tunnels accordingly. *)
    let projectb = new Postgresql.connection ~conninfo:"service=projectb" () in

    let mk_wrapper_maps transform sql =
      let r = projectb#exec sql in
      assert (r#status = Postgresql.Tuples_ok);
      Array.fold_left (fun (a, b) row ->
	match row with
        | [| key_id; key |] ->
          let key = transform key
          and key_id = int_of_string key_id in (
            IntMap.add key_id key a,
            StringMap.add key key_id b
          )
        | _ -> assert false
      ) (IntMap.empty, StringMap.empty) r#get_all
    in

    let string_identity x = x in

    let mk_wrappers name (key_of_id_map, id_of_key_map) =
      ((fun x ->
	try IntMap.find x key_of_id_map
	with Not_found -> ksprintf invalid_arg "%s_of_id(%d)" name x),
       (fun x ->
	 try StringMap.find x id_of_key_map
	 with Not_found -> ksprintf invalid_arg "id_of_%s(%s)" name x))
    in

    let key_of_id, id_of_key = mk_wrappers "key"
      (mk_wrapper_maps String.lowercase "select key_id, key from metadata_keys")
    in

    let suite_of_id, id_of_suite = mk_wrappers "suite"
      (mk_wrapper_maps string_identity "select id, suite_name from suite")
    in

    let arch_of_id, id_of_arch = mk_wrappers "arch"
      (mk_wrapper_maps string_identity "select id, arch_string from architecture")
    in

    let relevant_binary_key_ids = List.map id_of_key relevant_binary_keys in

    let get_binaries accu arch =
      Benl_clflags.progress "Querying projectb for %s binaries in staging (+ %s)..." arch suite;
      let sql = sprintf
	"select b.bin_id, b.key_id, b.value from bin_associations as a join (select * from binaries_metadata where key_id in (%s)) as b on b.bin_id = a.bin join (select * from binaries) as c on c.id = a.bin where a.suite = %d and c.architecture in (%d,%d)
	UNION ALL
	select b.bin_id, b.key_id, b.value from bin_associations as a join (select * from binaries_metadata where key_id in (%s)) as b on b.bin_id = a.bin join (select * from binaries) as c on c.id = a.bin where a.suite = %d and c.architecture in (%d,%d)"
	(String.concat "," (List.map string_of_int relevant_binary_key_ids))
	(id_of_suite "staging") (id_of_arch "all") (id_of_arch arch)
	(String.concat "," (List.map string_of_int relevant_binary_key_ids))
	(id_of_suite suite) (id_of_arch "all") (id_of_arch arch)
      in
      let r = projectb#exec sql in
      assert (r#status = Postgresql.Tuples_ok);
      let id_indexed_map = Array.fold_left (fun a row ->
	match row with
        | [| src_id; key_id; value |] ->
          let src_id = int_of_string src_id
          and key_id = int_of_string key_id in
          let old = try IntMap.find src_id a with Not_found -> [] in
          IntMap.add src_id ((key_of_id key_id, value)::old) a
        | _ -> assert false
      ) IntMap.empty r#get_all in
      let result = IntMap.fold (fun _ assoc accu ->
	let pkg = Package.of_assoc `binary assoc in
	let name = Package.Name.of_string (Package.get "package" pkg) in
	let ver = Package.get "version" pkg in
	try
          let old_pkg = PAMap.find (name, arch) accu in
          let old_ver = Package.get "version" old_pkg in
          if Benl_base.Version.compare old_ver ver < 0
          then PAMap.add (name, arch) pkg accu
          else accu
	with Not_found ->
          PAMap.add (name, arch) pkg accu
      ) id_indexed_map accu in
      Benl_clflags.progress "\n";
      result
    in

    let sources_in_testing =
      Benl_clflags.progress "Querying projectb for sources in %s..." suite;
      let sql = sprintf
	"select (select value from source_metadata as b where key_id = %d and b.src_id = a.source) from src_associations as a where a.suite = %d"
	(id_of_key "source") (id_of_suite suite)
      in
      let r = projectb#exec sql in
      assert (r#status = Postgresql.Tuples_ok);
      let result = Array.fold_left (fun a row ->
	match row with
        | [| source |] -> StringSet.add source a
        | _ -> assert false
      ) StringSet.empty r#get_all in
      Benl_clflags.progress "\n";
      result
    in

    let relevant_source_key_ids =
    (* beware! key "directory" does not exist in projectb and is
       handled specifically below *)
      List.map id_of_key
	(List.filter (fun x -> x <> "directory") relevant_source_keys)
    in

    let get_sources accu =
      Benl_clflags.progress "Querying projectb for sources in staging (+ %s)..." suite;
      (* get general metadata *)
      let sql = sprintf
	"select b.src_id, b.key_id, b.value from src_associations as a join (select * from source_metadata where key_id in (%s)) as b on b.src_id = a.source where a.suite = %d
	UNION ALL
	select b.src_id, b.key_id, b.value from src_associations as a join (select * from source_metadata where key_id in (%s)) as b on b.src_id = a.source where a.suite = %d"
	(String.concat "," (List.map string_of_int relevant_source_key_ids))
	(id_of_suite "staging")
	(String.concat "," (List.map string_of_int relevant_source_key_ids))
	(id_of_suite suite)
      in
      let r = projectb#exec sql in
      assert (r#status = Postgresql.Tuples_ok);
      let id_indexed_map = Array.fold_left (fun a row ->
	match row with
        | [| src_id; key_id; value |] ->
          let src_id = int_of_string src_id
          and key_id = int_of_string key_id in
          let old = try IntMap.find src_id a with Not_found -> [] in
          let key = key_of_id key_id in
          (* translate "source" to "package" for consistency with
             Sources files *)
          let key = if key = "source" then "package" else key in
          IntMap.add src_id ((key, value)::old) a
        | _ -> assert false
      ) IntMap.empty r#get_all in
    (* get .dsc paths to compute directories *)
      let sql = sprintf
	"select a.source, c.filename from src_associations as a join (select * from dsc_files) as b on b.source = a.source, files as c where (a.suite = %d or a.suite = %d) and b.file = c.id and c.filename like '%%dsc'"
	(id_of_suite "staging")
	(id_of_suite suite)
      in
      let r = projectb#exec sql in
      assert (r#status = Postgresql.Tuples_ok);
      let id_indexed_dscs = Array.fold_left (fun a row ->
	match row with
        | [| src_id; filename |] ->
          let src_id = int_of_string src_id in
          IntMap.add src_id filename a
        | _ -> assert false
      ) IntMap.empty r#get_all in
    (* fake directory entry by merging id_indexed_{map,dscs} *)
      let id_indexed_map = IntMap.mapi (fun src_id pkg ->
	let directory = Filename.concat "pool"
          (Filename.dirname (IntMap.find src_id id_indexed_dscs))
	in
	("directory", directory) :: pkg
      ) id_indexed_map in
      let result = IntMap.fold (fun _ assoc accu ->
	let pkg = Package.of_assoc `source assoc in
	let sname = Package.get "package" pkg in
	let is_in_testing =
          if StringSet.mem sname sources_in_testing
          then "yes" else "no"
	in
	let pkg = Package.add "is-in-testing" is_in_testing pkg in
	let name = Package.Name.of_string sname in
	let ver = Package.get "version" pkg in
	try
          let old_pkg = M.find name accu in
          let old_ver = Package.get "version" old_pkg in
          if Benl_base.Version.compare old_ver ver < 0
          then M.add name pkg accu
          else accu
	with Not_found ->
          M.add name pkg accu
      ) id_indexed_map accu in
      Benl_clflags.progress "\n";
      result
    in

    { get_binaries = get_binaries; get_sources = get_sources }

end

let filter_affected { src_map = srcs; bin_map = bins } is_affected =
  let src_map = M.fold begin fun name src accu ->
    if Query.eval_source src !!(is_affected ()) then
      M.add name src accu
    else accu
  end srcs M.empty in
  let src_map, bin_map = PAMap.fold begin fun (name, arch) pkg (saccu, baccu) ->
    let src_name = Package.get "source" pkg in
    let src_name = Package.Name.of_string src_name in
    try
      let src = M.find src_name srcs in
      if Query.eval_binary pkg !!(is_affected ())
      || Query.eval_source src !!(is_affected ())
      then begin
        M.add src_name src saccu
        ,
        PAMap.add (name, arch) pkg baccu;
      end
      else (saccu, baccu)
    with Not_found ->
      eprintf "warning: Binary (%s,%s) without Source!\n%!" !!!name arch;
      (saccu, baccu)
  end bins (src_map, PAMap.empty) in
  let bin_map = PAMap.fold (fun (name, arch) pkg accu ->
    let src_name = Package.get "source" pkg in
    let src_name = Package.Name.of_string src_name in
    if M.mem src_name src_map then PAMap.add (name, arch) pkg accu
    else accu
  ) bins bin_map in
  { src_map = src_map; bin_map = bin_map }

let inject_debcheck_data =
  let rex = Pcre.regexp "^  package: (.*)$" in
  fun (bins : [`binary] Package.t PAMap.t)  architectures ->
    let a, b = if !Benl_clflags.quiet then ("\n", "") else ("", "\n") in
    let all_uninstallable_packages = List.map (fun arch_ref ->
      Benl_clflags.progress "Running dose-debcheck on %s..." arch_ref;
      let (ic, oc) as p = Unix.open_process "dose-debcheck --quiet --failures" in
      (* inefficiency: for each architecture, we iterate on all binary
         packages, not only on binary packages of said architectures *)
      PAMap.iter (fun (name, arch) pkg ->
        if arch = arch_ref then Package.print oc pkg
      ) bins;
      close_out oc;
      let rec loop accu =
        begin match (try Some (input_line ic) with End_of_file -> None) with
          | None ->
            if Package.Set.is_empty accu then
              Printf.eprintf "W: no uninstallable packages!\n%!";
            accu
          | Some line ->
            try
              let r = Pcre.exec ~rex line in
              let package = Pcre.get_substring r 1 in
              loop (Package.Set.add (Package.Name.of_string package) accu)
            with Not_found -> loop accu
        end
      in
      let result = loop Package.Set.empty in
      begin match Unix.close_process p with
        | Unix.WEXITED (0|1) -> ()
        | Unix.WEXITED i ->
          Printf.eprintf
            "%sW: subprocess dose-debcheck exited with code %d%s%!" a i b
        | Unix.WSIGNALED i ->
          Printf.eprintf
            "%sW: subprocess dose-debcheck died with signal %d%s%!" a i b
        | Unix.WSTOPPED i ->
          Printf.eprintf
            "%sW: subprocess dose-debcheck stopped with signal %d%s%!" a i b
      end;
      Benl_clflags.progress "\n";
      (arch_ref, result)
    ) architectures in
    PAMap.mapi (fun (name, arch) pkg ->
      let uninstallable_packages = List.assoc arch all_uninstallable_packages in
      if Package.Set.mem name uninstallable_packages
      then
        (* the following line is for compatibility only and should be
           removed eventually *)
        let pkg = Package.add "edos-debcheck" "uninstallable" pkg in
        Package.add "uninstallable" "yes" pkg
      else pkg
    ) bins

let get_data is_affected =
  let file = Benl_clflags.get_cache_file () in
  if !Benl_clflags.use_cache && Sys.file_exists file then
    filter_affected (Marshal.load file) is_affected
  else
    let origin =
      if !use_projectb then Projectb.mk_origin () else file_origin
    in
    let src_raw = origin.get_sources M.empty in
    let bin_raw = List.fold_left
      origin.get_binaries PAMap.empty !Benl_clflags.architectures
    in
    let bin_raw = if !run_debcheck
      then inject_debcheck_data bin_raw !Benl_clflags.architectures
      else bin_raw
    in
    let data = { src_map = src_raw; bin_map = bin_raw; } in
    Marshal.dump file data;
    filter_affected data is_affected
