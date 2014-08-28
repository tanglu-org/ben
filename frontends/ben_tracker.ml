(**************************************************************************)
(*  Copyright © 2011 Mehdi Dogguy <mehdi@dogguy.org>                      *)
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

open Xhtml.M
open Printf
open Benl_core
open Benl_clflags
open Benl_utils
open Benl_error

module Arg = Benl_arg

let ($) f x = f x

let base = ref "."
let config_dir = ref "config"
let global_config = ref (FilePath.concat !config_dir "global.conf")
let lock = ref "ben.lock"
let clean = ref true
let tconfig = ref None

open Benl_types
open Benl_frontend

let read_global_config () =
  if Sys.file_exists !global_config then begin
    let config = Benl_utils.parse_config_file !global_config in
    StringMap.iter (fun key value -> match (key, value) with
        | "architectures", archs ->
          Benl_base.debian_architectures := to_string_l "architectures" archs
        | "ignored", archs ->
          Benl_base.ignored_architectures := to_string_l "ignored" archs
        | "suite", (EString suite) ->
          Benl_clflags.suite := suite
        | "areas", areas ->
          Benl_clflags.areas := to_string_l "areas" areas
        | "base", (EString path) ->
          base := path
        | "config-dir", (EString path) ->
          config_dir := path
        | "cache-dir", (EString dir) ->
          Benl_clflags.cache_dir := dir
        | "mirror-binaries", (EString mirror) ->
          Benl_clflags.mirror_binaries := mirror
        | "mirror-sources", (EString mirror) ->
          Benl_clflags.mirror_sources := mirror
        | "mirror", (EString mirror) ->
          Benl_clflags.mirror_sources := mirror;
          Benl_clflags.mirror_binaries := mirror
        | "use-cache", Etrue ->
          Benl_clflags.use_cache := true
        | "run-debcheck", Etrue ->
          Benl_data.run_debcheck := true
        | "use-projectb", Etrue ->
          Benl_data.use_projectb := true
        | "base-url", (EString url) ->
          Ben_monitor.baseurl := url
        | "template", (EString template) ->
          Benl_templates.load_template template;
        | item, _ ->
            warn (Unknown_configuration_item item)
    )
    config
  end

let lockf () =
  FilePath.concat !cache_dir !lock

let spec = Arg.align [
  "--config-dir" , Arg.Set_string config_dir, " Specify tracker's global configuration directory";
  "-cd"          , Arg.Set_string config_dir, " ";
  "--global-conf", Arg.Set_string global_config, " Specify tracker's global configuration file";
  "-g"           , Arg.Set_string global_config, " ";
  "--transition" , Arg.String (fun t -> tconfig := Some t), " Specify selected transition";
  "-t"           , Arg.String (fun t -> tconfig := Some t), " ";
  "--update"     , Arg.Set Benl_clflags.update, " Do update the cache file";
  "-u"           , Arg.Set Benl_clflags.update, " ";
  "--base"       , Arg.Set_string base, " Specify tracker's base directory";
  "-b"           , Arg.Set_string base, " ";
  "--use-projectb", Arg.Set Benl_data.use_projectb, " Use Projectb to fetch information about distributions";
  "--template"   , Arg.String (fun t -> Benl_templates.load_template t), " Specify template to use";
  "--no-clean"   , Arg.Clear clean, " Do not clean HTML directory before exiting";
]

exception Unknown_profile of string

type profile = Planned | Ongoing | Permanent | Finished | Old | Unknown

let string_of_profile = function
  | Planned -> "planned"
  | Ongoing -> "ongoing"
  | Permanent -> "permanent"
  | Finished -> "finished"
  | Old -> "old"
  | Unknown -> "unknown"

let profile_of_string = function
  | "planned" -> Planned
  | "ongoing" -> Ongoing
  | "permanent" -> Permanent
  | "finished" -> Finished
  | "old" -> Old
  | _ -> Unknown

let profiles_desc = [
  Planned   , ( "Some planned transitions"     , false );
  Ongoing   , ( "Ongoing transitions"          , true  );
  Permanent , ( "Permanent trackers"           , false );
  Finished  , ( "(almost) Finished transitions", true  );
  Old       , ( "Old trackers"                 , false );
  Unknown   , ( "Miscellaneous transitions"    , false );
]

open FileUtil

let p = Benl_clflags.progress

let is_packages_file name =
  try
    let name = String.sub (Filename.basename name) 0 9 in
    name = "Packages_"
  with _ -> false

let clear_cache () =
  let cached = !cache_dir in
  let cachef = !cache_file in
  let test_cond = Or (Basename_is cachef, Custom is_packages_file) in
  let pkgs = find test_cond cached (fun x y -> y ::x) [] in
  rm ~force:Force pkgs

let update_test () =
  let cachef = Benl_clflags.get_cache_file () in
     !Benl_clflags.update
  || (!Benl_clflags.use_cache && test (Not Exists) cachef)

let read_cache () =
  if update_test () then begin
    if not !Benl_data.use_projectb then begin
      clear_cache ();
      Ben_download.download_all ();
    end;
    Benl_data.generate_cache
      (Benl_clflags.get_cache_file ())
      !Benl_clflags.architectures
  end
  else
    Benl_data.load_cache ()

let profile_of_file file =
  try
    profile_of_string $ Filename.basename (Filename.dirname file)
  with _ -> Unknown

let read_transition_config file =
  let (!!) = Filename.basename in
  let transition = FilePath.chop_extension !!file in
  (* Read a .ben file *)
  transition, (Benl_frontend.read_ben_file file)

let get_transition_data data name config =
  try
    let transition_data = Ben_monitor.compute_transition_data data config in
    let monitor_data, sources, binaries, dep_graph, all, bad, packages =
      transition_data in
    let has_testing_data = Ben_monitor.has_testing_data monitor_data in
    let export =
      try
        Benl_clflags.get_config config "export" = Benl_types.Etrue
      with _ -> true in
    Some (export, transition_data, has_testing_data)
  with e ->
    Benl_error.warn_exn ("Failed to process transition " ^ name) e;
    None

module SMap = Map.Make(String)

let sadd mp p t =
  let ts =
    try SMap.find p mp
    with _ -> [] in
  SMap.add p (t::ts) mp

let smerge _ v1 v2 = match v1, v2 with
  | Some v1, Some v2 -> Some (v1 @ v2)
  | Some v1, None    -> Some v1
  | None   , Some v2 -> Some v2
  | _                -> None

let html_path_t name =
  let htmlf = FilePath.replace_extension name "html" in
  Filename.concat "html" htmlf

let print_html_collisions (hits : (SMap.key * SMap.key list) list) =
  let hits = List.fold_left
    (fun map (pkg, transitions) ->
      List.fold_left
        (fun map t -> sadd map t pkg)
        map
        transitions
    )
    SMap.empty
    hits
  in
  div ~a:[ a_id "collisions" ] [
    b [ pcdata "Collisions:" ];
    let hits =
      SMap.fold
        (fun transition packages list ->
          let elt =
            li [ Ben_monitor.a_link
                   (FilePath.replace_extension transition "html")
                   transition;
                 pcdata " through ";
                 pcdata (String.concat ", " packages)
               ]
          in
          elt :: list
        )
        hits
        []
    in
    match hits with
    | [] -> pcdata "(none)"
    | h::l -> ul h l
  ]

let print_html_monitor config template file transition_data has_testing_data collisions =
  let monitor_data, sources, binaries, dep_graph, _, _, packages =
    transition_data
  in
  let ($) = Filename.concat in
  let (!!) = Filename.basename in
  let collisions_div =
    try
      let hits = SMap.find !!file collisions in
      Some (print_html_collisions hits)
    with Not_found ->
      None
  in
  let output =
    Ben_monitor.print_html_monitor
      config
      template
      monitor_data
      sources
      binaries
      dep_graph
      packages
      has_testing_data
      collisions_div
  in
  let htmlp = html_path_t !!file in
  let html = !base $ htmlp in
  p "Generating %s\n" htmlp;
  try
    Benl_utils.dump_xhtml_to_file html output
  with e ->
    Benl_error.warn_exn ("Failed to generate" ^ html) e

let compute_collisions results =
  let data_map = List.fold_left
    (fun
      data_map
      (_, transition, _, (_, transition_data, has_testing_data)) ->
        let  _, _, _, _, _, _, pkgs = transition_data in
        let new_data = Benl_data.S.fold
          (fun package data ->
            SMap.add (Package.Name.to_string package) [transition] data
          )
          pkgs
          SMap.empty
        in
        SMap.merge smerge data_map new_data
    )
    SMap.empty
    results
  in
  let collision_map = SMap.fold
    (fun pkg transitions map ->
      List.fold_left
        (fun map t ->
          let ts_left = List.filter (fun r -> t <> r) transitions in
          sadd map t (pkg,ts_left)
        )
        map
        transitions
    )
    data_map
    SMap.empty
  in
  collision_map

let generate_stats results =
  List.fold_left
    (fun (packages, profiles)
      (p, t, export, transition_data, _) ->
        let _, _, _, _, all, bad, pkgs = transition_data in
        let htmlp = html_path_t t in
        let profiles = sadd
          profiles
          (string_of_profile p)
          (htmlp, t, all, bad)
        in
        let packages = Benl_data.S.fold
          (fun package packages ->
            if export then
              sadd
                packages
                (Package.Name.to_string package)
                (t, p, export)
            else
              packages
          )
          pkgs
          packages
        in
        packages, profiles
    )
    (SMap.empty, SMap.empty)
    results

let dump_yaml smap file =
  let transition (name, profile, _) =
    sprintf "[ '%s' , '%s' ]"
      name
      (string_of_profile profile)
  in
  let file = Filename.concat !base (Filename.concat "export" file) in
  p "Generating %s\n" file;
  let string = SMap.fold
    (fun key list string ->
      let list = List.filter (fun (_,_,export) -> export) list in
      sprintf "%s- {'name': '%s',\n   'list': [%s]\n  }\n"
        string
        key
        (String.concat ", " (List.map transition list))
    )
    smap
    ""
  in
  try
    mkdir ~parent:true (Filename.dirname file);
    let newfile = FilePath.add_extension file "new" in
    dump_to_file newfile string;
    mv newfile file
  with exn ->
    Benl_error.error_exn ("Failed to generate " ^ file) exn

let clean_up smap =
  if !clean then begin
    p "Cleaning up...\n";
    let ($) = Filename.concat in
    let html_dir = !base $ "html" in
    let known_transitions =
      SMap.fold
        (fun _ tlist accu ->
          let tlist = List.map
            (fun (name, _, _, _) -> Filename.basename name)
            tlist
          in
          let tset = StringSet.from_list tlist in
          StringSet.union tset accu
        )
        smap
        StringSet.empty
    in
    try
      let dir_content = Sys.readdir html_dir in
      Array.iter
        (fun file ->
          if Filename.check_suffix file ".html" &&
            not (StringSet.mem file known_transitions)
          then begin
            let file = html_dir $ file in
            p "Removing %s\n" file;
            Sys.remove file
          end
        )
        dir_content
    with _ -> ()
  end

let tracker template profiles =
  let page_title = "Transition tracker" in
  let footer = [ small (Ben_monitor.generated_on_text ()) ] in
  let tget show_score (path, name, all, bad) =
    li (
      (Ben_monitor.a_link path name)::
      if show_score then
        let score =
          if all = 0
          then 0
          else 100*(all-bad)/all in
        [ pcdata (sprintf " (%d%%)" score) ]
      else []
    )
  in
  let contents = SMap.fold
    (fun profile tlist acc ->
      let title, show_score =
        try
          let profile = profile_of_string profile in
          List.assoc profile profiles_desc
        with _ ->
          List.assoc Unknown profiles_desc
      in
      let tlist = List.sort Pervasives.compare tlist in
      let tlist = List.map (tget show_score) tlist in
      match tlist with
        | [] -> acc
        | h::l ->
          let tdiv = div ~a:[ a_class [ "transitions" ] ]
            [ b [ pcdata title ];
              ul h l
            ] in
          tdiv::acc
    )
    profiles
    []
  in
  let contents = template.Template.intro @ contents in
  let index = Filename.concat !base "index.html" in
  let subtitle = [ pcdata "Transition tracker" ] in
  let output = template.Template.page page_title subtitle [] contents footer in
  try
    p "Generating index...\n";
    dump_xhtml_to_file index output
  with exn ->
    Benl_error.error_exn "Failed to generate index.html" exn

let () = at_exit (fun () ->
  rm [lockf ()]
)

let main args =
  let () = read_global_config () in
  let lockf = lockf () in
  if test Exists lockf then
    eprintf "Please wait until %s is removed!\n" lockf
  else
    try
      let lockf_b = Filename.dirname lockf in
      let () = if test Exists lockf_b then
          touch lockf
        else
          eprintf "%s doesn't exist. Skipping creation of lock file.\n" lockf_b
      in
      let htmld = Filename.concat !base "html" in
      if test (Not Exists) htmld then
        mkdir ~parent:true htmld;
      Ben_monitor.check_media_dir !base;
      let confd = !config_dir in
      let test_cond = And (Size_not_null,
                      And (Has_extension "ben",
                      And (Is_file, Is_readable))) in
      let template = Benl_templates.get_registered_template () in
      (* Computing list of transitions *)
      let transition_files =
        match !tconfig with
        | Some transition -> [transition, profile_of_file transition]
        | None ->
          find test_cond confd
            (fun results transition ->
              match profile_of_file transition with
              | Old -> results
              | profile -> (transition, profile) :: results
            )
            []
      in
      (* Read found .ben files *)
      let transitions =
        Benl_parallel.map
          (fun (transition, profile) ->
            try
              let name, config = read_transition_config transition in
              Some (name, (config, transition, profile))
            with
            | Benl_error.Error e -> (* Ben file has errors *)
              warn e;
              None
            | e ->
              Benl_error.warn_exn ("Failed to read " ^ transition) e;
              None

          )
          transition_files
      in
      let transitions =
        List.fold_left
          (fun transitions -> function
          | Some t -> t :: transitions
          | None -> transitions
          )
          []
          transitions
      in
      (* Read ben.cache *)
      let cache = read_cache () in
      (* Compute data for each transition *)
      let results =
        Benl_parallel.map
          (fun (name, (config, file, profile)) ->
            let () =
              p "Computing data for (%s) %s\n"
                (string_of_profile profile)
                name
            in
            config, name, profile, (get_transition_data cache name config)
          )
          transitions
      in
      let results =
        List.fold_left
          (fun results -> function
          | (c, n, p, Some d) -> (c, n, p, d) :: results
          | _ -> results
          )
          []
          results
      in
      (* Compute collisions *)
      let collisions = compute_collisions results in
      (* Generate an HTML page for each transition *)
      let () = Benl_parallel.iter
        (fun (config, transition, _, (_, transition_data, has_testing_data)) ->
          print_html_monitor
            config
            template
            transition
            transition_data
            has_testing_data
            collisions
        )
        results
      in
      (* Generate the packages.yaml file *)
      let results =
        Benl_parallel.map
          (fun (config, t, p, (export, transition_data, has_testing_data)) ->
            p, t, export, transition_data, has_testing_data
          )
          results
      in
      let packages, profiles = generate_stats results in
      let () = dump_yaml packages "packages.yaml" in
      (* Clean up the HTML directory from old files *)
      let () = clean_up profiles in
      (* Generate the index page *)
      (match !tconfig with
        | None -> tracker template profiles
        | Some _ -> ())
    with exn ->
      Benl_error.error_exn "General error" exn

let frontend = {
  Benl_frontend.name = "tracker";
  Benl_frontend.main = main;
  Benl_frontend.anon_fun = (fun _ -> ());
  Benl_frontend.help = spec;
}
