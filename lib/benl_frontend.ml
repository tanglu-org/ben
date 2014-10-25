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

open Benl_error
open Benl_core
open Benl_types

module Arg = Benl_arg

type frontend = {
  name : string;
  main : unit -> unit;
  anon_fun: string -> unit;
  help : (Arg.key * Arg.spec * Arg.doc) list
}
let frontends = ref []
let current_frontend = ref None

let register_frontend sc =
  frontends := (sc.name, sc) :: !frontends

let get_frontend x =
  try List.assoc x !frontends
  with Not_found -> raise (Unknown_command x)

let get_selected_frontend () =
  match !current_frontend with
  | None -> Pervasives.raise (Arg.Help "No frontend selected!")
  | Some frontend -> frontend

let set_selected_frontend x =
  current_frontend := Some x

let available_frontends () =
  List.map fst !frontends

let to_cmd x =
  let n = String.length x in
  if n > 4 && String.sub x 0 4 = "ben-" then
    String.sub x 4 (n-4)
  else x

let fail fmt = Printf.ksprintf
  (fun x -> raise (Error_in_configuration_file x))
  fmt

let to_string what = function
  | EString s -> s
  | _ -> fail "%s must be a string" what

let to_string_l what = function
  | EList ys ->
      List.map begin function
        | EString s -> s
        | _ -> fail "%s must be a list of strings" what
      end ys
  | _ -> fail "%s must be a list of strings" what

let to_expr_l l =
  let of_string s = Benl_types.EString s in
  Benl_types.EList (List.map of_string l)

let read_config ?(multi = false) source =
  let config = match source with
    | File filename -> Benl_utils.parse_config_file filename
    | Stdin -> Benl_utils.parse_config_from_in_channel stdin
    | NoSource -> assert false
  in
  StringMap.fold (fun key value accu -> match (key, value) with
    | ("mirror", x) ->
        Benl_clflags.mirror_binaries := to_string "mirror" x;
        Benl_clflags.mirror_sources  := to_string "mirror" x;
        StringMap.add key value accu
    | ("mirror-binaries", x) ->
        Benl_clflags.mirror_binaries := to_string "mirror-binaries" x;
        StringMap.add key value accu
    | ("mirror-sources", x) ->
        Benl_clflags.mirror_sources := to_string "mirror-sources" x;
        StringMap.add key value accu
    | ("areas", x) ->
        if multi then
          StringMap.add key value accu
        else
          let () = Benl_clflags.areas := to_string_l key x in
          accu
    | ("architectures", x) ->
        if multi then
          StringMap.add key value accu
        else
          let () = Benl_clflags.architectures := to_string_l key x in
          accu
    | ("suite", x) ->
        if multi then
          StringMap.add key value accu
        else
          let () = Benl_clflags.suite := to_string key x in
          accu
    | ("cache-dir", x) ->
        Benl_clflags.cache_dir := to_string "cache-dir" x;
        StringMap.add key value accu
    | ("cache-file", x) ->
        let name = to_string "cache-file" x in
        Benl_clflags.set_cache_file name;
        accu
    | ("use-cache", Etrue) ->
        Benl_clflags.use_cache := true;
        StringMap.add key value accu
    | ("more-binary-keys", x) ->
        let new_keys = List.map
          String.lowercase
          (to_string_l "more-binary-keys" x)
        in
        Benl_data.relevant_binary_keys := StringSet.union
          (StringSet.from_list new_keys)
          !Benl_data.relevant_binary_keys;
        accu
    | ("more-source-keys", x) ->
        let new_keys = List.map
          String.lowercase
          (to_string_l "more-source-keys" x)
        in
        Benl_data.relevant_source_keys := StringSet.union
          (StringSet.from_list new_keys)
          !Benl_data.relevant_source_keys;
        accu
    | ("preferred-compression-format", x) ->
        let format = to_string "preferred-compression-format" x in
        if Benl_compression.is_known format then
          Benl_clflags.preferred_compression_format :=
            Benl_compression.of_string format
        else
          warn (Unknown_input_format format);
        StringMap.add key value accu
    | _ ->
        StringMap.add key value accu
  )
  config
  StringMap.empty

let read_ben_file filename =
  let config = read_config ~multi:true (File filename) in
  let default_values = [
    "architectures", to_expr_l !Benl_clflags.architectures;
    "ignored", to_expr_l !Benl_base.ignored_architectures;
    "areas", to_expr_l !Benl_clflags.areas;
    "suite", Benl_types.EString !Benl_clflags.suite;
  ] in
  List.fold_left
    (fun config (key, value) ->
      if not (StringMap.mem key config) then
        StringMap.add key value config
      else
        config
    )
    config
    default_values

let spec = ref (Arg.align [
  "--no-benrc", Arg.Clear Benl_clflags.use_benrc, " Do not read .benrc file at startup";
  "--dry-run" , Arg.Set Benl_clflags.dry_run, " Dry run";
  "-n"        , Arg.Set Benl_clflags.dry_run, " Dry run";
  "--parallel", Arg.Int Benl_parallel.set_level, " Set number of cores to use";
  "-P"        , Arg.Int Benl_parallel.set_level, " Set number of cores to use";
  "--quiet"   , Arg.Set Benl_clflags.quiet, " Quiet mode";
  "-q"        , Arg.Set Benl_clflags.quiet, " Quiet mode";
  "--verbose" , Arg.Set Benl_clflags.verbose, " Verbose mode";
  "-v"        , Arg.Set Benl_clflags.verbose, " Verbose mode";
  "--mirror"  , Arg.String (fun m ->
    Benl_clflags.mirror_binaries := m;
    Benl_clflags.mirror_sources := m
  )                                         , " Mirror to use";
  "--mirror-binaries" , Arg.String (fun m ->
    Benl_clflags.mirror_binaries := m
  )                                         , " Mirror to use for binaries";
  "--mirror-sources"  , Arg.String (fun m ->
    Benl_clflags.mirror_sources := m
  )                                         , " Mirror to use for sources";
  "--areas"   , Arg.String (fun a ->
    Benl_clflags.areas := Benl_core.simple_split ',' a)
                                            , " Areas to consider";
  "--archs"   , Arg.String (fun a ->
    Benl_clflags.architectures := Benl_core.simple_split ',' a)
                                            , " Architectures to consider";
  "--suite"   , Arg.Set_string Benl_clflags.suite, " Suite";
  "--cache-dir", Arg.Set_string Benl_clflags.cache_dir, " Path to cache directory";
  "--config"  , Arg.String (fun c ->
    ignore (read_config (File c)))
                                            , " Path to configuration file";
  "-c"  , Arg.String (fun c ->
    ignore (read_config (File c)))
                                            , " Path to configuration file";
  "--cache"   , Arg.String Benl_clflags.set_cache_file, " Path to cache file";
  "-C"        , Arg.String Benl_clflags.set_cache_file, " Path to cache file";
  "--use-cache", Arg.Set Benl_clflags.use_cache, " Enable use of cache file, if available";
  "--more-binary-keys", Arg.String (fun x ->
    let new_keys = List.map
      String.lowercase
      (Benl_core.simple_split ',' x)
    in
    Benl_data.relevant_binary_keys := StringSet.union
      (StringSet.from_list new_keys)
      !Benl_data.relevant_binary_keys)
                                                , " Further relevant binary keys";
  "--more-source-keys", Arg.String (fun x ->
    let new_keys = List.map
      String.lowercase
      (Benl_core.simple_split ',' x)
    in
    Benl_data.relevant_source_keys := StringSet.union
      (StringSet.from_list new_keys)
      !Benl_data.relevant_source_keys)
                                                , " Further relevant source keys";
  "--preferred-compression-format", Arg.String (fun x ->
    if Benl_compression.is_known x then
      Benl_clflags.preferred_compression_format := Benl_compression.of_string x
    else
      warn (Unknown_input_format x))
                                                , " Preferred compression format";
  "-z", Arg.String (fun x ->
    if Benl_compression.is_known x then
      Benl_clflags.preferred_compression_format := Benl_compression.of_string x
    else
      warn (Unknown_input_format x))
                                                , " Preferred compression format";
  "-h", Arg.Unit (fun () -> Pervasives.raise (Arg.Help "Use -help or --help instead\n")), " Display this list of options";
  "-V", Arg.Unit (fun () -> Benl_clflags.show_version := true), " Display version number (and build date) and exists.";
  "--version", Arg.Set Benl_clflags.show_version, " Display version number (and build date) and exists.";
])
