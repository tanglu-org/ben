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

type frontend = {
  name : string;
  main : string list -> unit;
  help : unit -> unit
}
let frontends = ref []

let use_benrc = ref true

let register_frontend sc =
  frontends := (sc.name, sc) :: !frontends

let get_frontend x =
  try List.assoc x !frontends
  with Not_found -> raise (Unknown_command x)

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

let check_string what = function
  | EString s -> s
  | _ -> fail "%s must be a string" what

let check_string_list what = function
  | EList ys ->
      List.map begin function
        | EString s -> s
        | _ -> fail "%s must be a list of strings" what
      end ys
  | _ -> fail "%s must be a list of strings" what

let read_config_file filename =
  let config = Benl_utils.parse_config_file filename in
  StringMap.fold (fun key value accu -> match (key, value) with
    | ("mirror", x) ->
        Benl_clflags.mirror_binaries := check_string "mirror" x;
        Benl_clflags.mirror_sources  := check_string "mirror" x;
        accu
    | ("mirror-binaries", x) ->
        Benl_clflags.mirror_binaries := check_string "mirror-binaries" x;
        accu
    | ("mirror-sources", x) ->
        Benl_clflags.mirror_sources := check_string "mirror-sources" x;
        accu
    | ("areas", x) ->
        Benl_clflags.areas := check_string_list "areas" x;
        accu
    | ("architectures", x) ->
        Benl_clflags.architectures := check_string_list "architectures" x;
        accu
    | ("suite", x) ->
        Benl_clflags.suite := check_string "suite" x;
        accu
    | ("cache-dir", x) ->
        Benl_clflags.cache_dir := check_string "cache-dir" x;
        accu
    | ("cache-file", x) ->
        Benl_clflags.cache_file := check_string "cache-file" x;
        accu
    | ("use-cache", Etrue) ->
        Benl_clflags.use_cache := true;
        accu
    | ("more-binary-keys", x) ->
        let new_keys = List.map
          String.lowercase
          (check_string_list "more-binary-keys" x)
        in
        Benl_data.relevant_binary_keys := StringSet.union
          (StringSet.from_list new_keys)
          !Benl_data.relevant_binary_keys;
        accu
    | ("more-source-keys", x) ->
        let new_keys = List.map
          String.lowercase
          (check_string_list "more-source-keys" x)
        in
        Benl_data.relevant_source_keys := StringSet.union
          (StringSet.from_list new_keys)
          !Benl_data.relevant_source_keys;
        accu
    | ("preferred-compression-format", x) ->
        let format = check_string "preferred-compression-format" x in
        if Benl_compression.is_known format then
          Benl_clflags.preferred_compression_format :=
            Benl_compression.of_string format
        else
          warn (Unknown_input_format format);
        accu
    | _ ->
        StringMap.add key value accu
  )
  config
  StringMap.empty

let rec parse_common_args = function
  | "--no-benrc"::xs ->
      use_benrc := false;
      parse_common_args xs
  | ("--dry-run" | "-n")::xs ->
      Benl_clflags.dry_run := true;
      parse_common_args xs
  | ("--quiet" | "-q")::xs ->
      Benl_clflags.quiet := true;
      parse_common_args xs
  | ("--verbose" | "-v")::xs ->
      Benl_clflags.verbose := true;
      parse_common_args xs
  | "--mirror"::x::xs ->
      Benl_clflags.mirror_binaries := x;
      Benl_clflags.mirror_sources := x;
      parse_common_args xs
  | "--mirror-binaries"::x::xs ->
      Benl_clflags.mirror_binaries := x;
      parse_common_args xs
  | "--mirror-sources"::x::xs ->
      Benl_clflags.mirror_sources := x;
      parse_common_args xs
  | "--areas"::x::xs ->
      Benl_clflags.areas := Benl_core.simple_split ',' x;
      parse_common_args xs
  | "--archs"::x::xs ->
      Benl_clflags.architectures := Benl_core.simple_split ',' x;
      parse_common_args xs
  | "--suite"::x::xs ->
      Benl_clflags.suite := x;
      parse_common_args xs
  | "--cache-dir"::x::xs ->
      Benl_clflags.cache_dir := x;
      parse_common_args xs
  | ("--config"|"-c")::x::xs ->
      Benl_clflags.config := read_config_file x;
      parse_common_args xs
  | ("--cache"|"-C")::x::xs ->
      Benl_clflags.cache_file := x;
      parse_common_args xs
  | "--use-cache"::xs ->
      Benl_clflags.use_cache := true;
      parse_common_args xs
  | "--more-binary-keys"::x::xs ->
      let new_keys = List.map
        String.lowercase
        (Benl_core.simple_split ',' x)
      in
      Benl_data.relevant_binary_keys := StringSet.union
        (StringSet.from_list new_keys)
        !Benl_data.relevant_binary_keys;
      parse_common_args xs
  | "--more-source-keys"::x::xs ->
      let new_keys = List.map
        String.lowercase
        (Benl_core.simple_split ',' x)
      in
      Benl_data.relevant_source_keys := StringSet.union
        (StringSet.from_list new_keys)
        !Benl_data.relevant_source_keys;
      parse_common_args xs
  | ("--preferred-compression-format"|"-z")::x::xs ->
      if Benl_compression.is_known x then
        Benl_clflags.preferred_compression_format := Benl_compression.of_string x
      else
        warn (Unknown_input_format x);
      parse_common_args xs
  | x::xs -> x::(parse_common_args xs)
  | [] -> []

let print_help () =
  Printf.printf "Usage: %s command [options]\n%!" Sys.argv.(0);
  Printf.printf "List of available commands:\n%!";
  List.iter
    (fun frontend ->
      Printf.printf " - %s\n%!" frontend;
      (get_frontend frontend).help ()
    )
    (available_frontends ());
  Printf.printf "List of available options:\n%!";
  List.iter
    (fun (option, desc) ->
      Printf.printf " * %s\t%s\n%!" option desc
    )
    [ "--dry-run" , "Dry run";
      "--quiet|-q", "Quiet mode";
      "--verbose", "Verbose mode";
      "--mirror", "Mirror to use";
      "--mirror-binaries", "Mirror to use for binaries";
      "--mirror-sources", "Mirror to use for sources";
      "--areas", "Areas to consider";
      "--archs", "Architectures to consider";
      "--suite", "Suite";
      "--cache-dir", "Path to cache dir";
      "--cache|-C", "Path to cache file";
      "--use-cache", "Use cache file if available";
      "--config|-c", "Config file";
      "--more-binary-keys", "Further relevant binary keys";
      "--more-source-keys", "Further relevant source keys";
      "--preferred-compression-format|-z", "Preferred compression format";
    ];
  exit 0

let main () = match Array.to_list Sys.argv with
  | [] ->
      (* we assume Sys.argv.(0) is always here! *)
      assert false
  | _ :: ("-h"|"-help"|"--help") :: _ ->
      print_help ()
  | cmd::xs ->
      let sc, args =
        try
          let cmd = to_cmd (Filename.basename cmd) in
          get_frontend cmd, xs
        with Error (Unknown_command _) -> match xs with
          | cmd::xs ->
              let benrc = Filename.concat (Sys.getenv "HOME") ".benrc" in
              if !use_benrc && Sys.file_exists benrc then
                ignore (read_config_file benrc);
              get_frontend cmd, xs
          | _ ->
              print_help ()
      in
      Printexc.catch sc.main args
