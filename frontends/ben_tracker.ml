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
open Benl_clflags
open Benl_utils
open Ben_monitor
open Benl_error

let ($) f x = f x

let base = ref "."
let config_dir = ref "config"
let global_config = ref (FilePath.concat !config_dir "global.conf")
let cache_file = ref "monitor.cache"
let lock = ref "ben.lock"
let update = ref false
let tconfig = ref None

open Benl_types
open Benl_frontend

let read_global_config () =
  if Sys.file_exists !global_config then begin
    let config = Benl_utils.parse_config_file !global_config in
    List.iter (function
        | "architectures", archs ->
          Benl_base.debian_architectures := check_string_list "architectures" archs
        | "ignored", archs ->
          Benl_base.ignored_architectures := check_string_list "ignored" archs
        | "suite", (EString suite) ->
          Benl_clflags.suite := suite
        | "areas", areas ->
          Benl_clflags.areas := check_string_list "areas" areas
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
          Ben_monitor.use_cache := true
        | "run-debcheck", Etrue ->
          Ben_monitor.run_debcheck := true
        | "use-projectb", Etrue ->
          Ben_monitor.use_projectb := true
        | "base-url", (EString url) ->
          Ben_monitor.baseurl := url
        | "template", (EString template) ->
          Benl_templates.load_template template;
        | "output-type", (EString format) ->
          (match String.lowercase format with
            | "text" -> Ben_monitor.output_type := Text
            | "levels" -> Ben_monitor.output_type := Levels
            | "xhtml" -> Ben_monitor.output_type := Xhtml
            | format ->
                warn (Unknown_output_format format);
                Ben_monitor.output_type := Xhtml
          )
        | item, _ ->
            warn (Unknown_configuration_item item)
    )
    config
  end

let lockf () =
  FilePath.concat !cache_dir !lock

let rec parse_local_args = function
  | ("--config-dir"|"-cd")::x::xs ->
      config_dir := x;
      parse_local_args xs
  | ("--global-conf"|"-g")::x::xs ->
      global_config := x;
      parse_local_args xs
  | ("--transition"|"-t")::x::xs ->
      tconfig := Some x;
      parse_local_args xs
  | ("--update"|"-u")::xs ->
      update := true;
      parse_local_args xs
  | ("--base"|"-b")::x::xs ->
      base := x;
      parse_local_args xs
  | ("--use-projectb")::xs ->
      Ben_monitor.use_projectb := true;
      parse_local_args xs
  | "--template"::template::xs ->
      Benl_templates.load_template template;
      parse_local_args xs
  | x::xs -> x::(parse_local_args xs)
  | [] -> []

let help () =
  List.iter
    (fun (option , desc) ->
      printf "    %s: %s\n%!" option desc
    )
    [ "--base|-b [dir]", "Specifies the \"base\" directory.";
      "--config-dir|-cd [dir]", "Location of ben trackers";
      "--transition|-t [profile/transition]", "Generate only that tracker page";
      "--update|-u", "Updates cache files";
      "--use-projectb", "Get package lists from Projectb database";
      "--template", "Select an HTML template";
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
  let cachef = Filename.concat !cache_dir !cache_file in
     !update
  || test (Not Exists) cachef

let update_cache () =
  let () = clear_cache () in
  if not !Ben_monitor.use_projectb then Ben_download.download_all ()

let profile_of_file file =
  try
    profile_of_string $ Filename.basename (Filename.dirname file)
  with _ -> Unknown

let run_monitor template file =
  let ($) = Filename.concat in
  let (!!) = Filename.basename in
  let profile = profile_of_file file in
  let transition = FilePath.chop_extension !!file in
  let () = p "Generating (%s) %s\n" (string_of_profile profile) transition in
  (* Reset config variables before reading .ben files *)
  let () = Benl_clflags.reset () in
  (* Read a .ben file *)
  let () = Benl_clflags.config := Benl_frontend.read_config_file file in
  let rounds, sources, binaries, dep_graph = compute_graph () in
  let all, bad, packages, output =
    Ben_monitor.print_html_monitor template sources binaries dep_graph rounds in
  let htmlf = FilePath.replace_extension !!file "html" in
  let htmlp = "html" $ htmlf in
  let html = !base $ htmlp in
  let export =
    try
      Benl_clflags.get_config "export" = Benl_types.Etrue
    with _ -> true in
  let result = all, bad, htmlp, profile, transition, packages, export in
  try
    Benl_utils.dump_xhtml_to_file html output;
    result
  with _ ->
    eprintf "Something bad happened while generating %s!\n" html;
    result

module SMap = Map.Make(String)

let sadd mp p t =
  let ts =
    try SMap.find p mp
    with _ -> [] in
  SMap.add p (t::ts) mp

let generate_stats results =
  List.fold_left
    (fun (packages, profiles)
      (all, bad, htmlp, p, t, pkgs, export) ->
        let pkgs = List.map Package.Name.to_string pkgs in
        let profiles = sadd
          profiles
          (string_of_profile p)
          (htmlp, t, all, bad)
        in
        let packages = List.fold_left
          (fun packages package ->
            if export then
              sadd packages package (t, p, export)
            else
              packages
          )
          packages
          pkgs in
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
  with exc ->
    eprintf "E: %s\n" $ Printexc.to_string exc;
    Printexc.print_backtrace stderr;
    eprintf "Something bad happened while generating %s!\n" file

let tracker template profiles =
  let page_title = "Transition tracker" in
  let footer = [ small (generated_on_text ()) ] in
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
  with exc ->
    eprintf "E: %s\n" $ Printexc.to_string exc;
    Printexc.print_backtrace stderr;
    eprintf "Something bad happened while generating index.html!\n"

let () = at_exit (fun () ->
  rm [lockf ()]
)

let main args =
  let _ = parse_local_args (Benl_frontend.parse_common_args args) in
  let () = read_global_config () in
  let lockf = lockf () in
  if test Exists lockf then
    eprintf "Please wait until %s is removed!\n" lockf
  else
    try
      touch lockf;
      if update_test ()  then update_cache ();
      let htmld = Filename.concat !base "html" in
      if test (Not Exists) htmld then
        mkdir ~parent:true htmld;
      Ben_monitor.check_media_dir !base;
      let confd = !config_dir in
      let test_cond = And (Size_not_null,
                      And (Has_extension "ben",
                      And (Is_file, Is_readable))) in
      let template = Benl_templates.get_registered_template () in
      let results =
        match !tconfig with
        (* Here we suppose that config is relative to base directory *)
          | Some transition ->
            let transition = Filename.concat !config_dir transition in
            [ run_monitor template transition ]
          | None ->
            find test_cond confd
              (fun results transition ->
                match profile_of_file transition with
                  | Old -> results
                  | _ ->
                    try
                      let result = run_monitor template transition in
                      result :: results
                    with Benl_error.Error e -> (* Ben file has errors *)
                      warn e;
                      results
              )
              []
      in
      (* Should we yell if all .ben files were broken? i.e. results == [] *)
      let packages, profiles = generate_stats results in
      let () = dump_yaml packages "packages.yaml" in
      (match !tconfig with
        | None -> tracker template profiles
        | Some _ -> ())
    with exc ->
      eprintf "E: %s\n" $ Printexc.to_string exc;
      Printexc.print_backtrace stderr

let frontend = {
  Benl_frontend.name = "tracker";
  Benl_frontend.main = main;
  Benl_frontend.help = help;
}
