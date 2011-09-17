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
open Benl_core
open Benl_base
open Benl_marshal
open XHTML.M

module M = Package.Map
module S = Package.Set

let use_cache = ref false
let use_colors = ref false
let run_debcheck = ref false

type output_type = Text | Xhtml | Levels
let output_type = ref Levels

let p = Benl_clflags.progress
let ( // ) = Filename.concat
let ( !! ) = Lazy.force
let ( !!! ) = Package.Name.to_string

let is_affected () =
  lazy (Query.of_expr (Benl_clflags.get_config "is_affected"))
let is_good () =
  lazy (Query.of_expr (Benl_clflags.get_config "is_good"))
let is_bad () =
  lazy (Query.of_expr (Benl_clflags.get_config "is_bad"))
let to_forget = List.fold_left
  (fun accu x -> Benl_base.Fields.add x accu) Benl_base.Fields.empty
  [
    "description";
  ]

module PAIndex = struct
  type t = [`binary] Package.Name.t * string
  let compare = Pervasives.compare
end
module PAMap = Map.Make(PAIndex)

module Marshallable = struct
  let magic_number = "BENA0901"
  type t = {
    src_map : ([`source], [`source] Package.t) Package.Map.t;
    bin_map : [`binary] Package.t PAMap.t
  }
end
module Marshal = Benl_marshal.Make(Marshallable)
open Marshallable

let format_arch x =
  let f = match x with
    | Unknown -> (fun x -> "."^x^".")
    | Up_to_date -> (fun x -> "("^x^")")
    | Outdated -> (fun x -> "["^x^"]")
  in
  let f =
    if !use_colors then
      match x with
        | Unknown -> f
        | Up_to_date -> (fun x -> "\027[32m"^(f x)^"\027[0m")
        | Outdated -> (fun x -> "\027[1m\027[31m"^(f x)^"\027[0m")
    else f
  in f

let parse_binaries accu arch =
  Benl_utils.parse_control_file `binary
    (!Benl_clflags.cache_dir // ("Packages_"^arch))
    !run_debcheck
    to_forget
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

let parse_sources accu =
  Benl_utils.parse_control_file `source
    (!Benl_clflags.cache_dir // "Sources")
    !run_debcheck
    to_forget
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

let filter_affected { src_map = srcs; bin_map = bins } =
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
  { src_map = src_map; bin_map = bin_map }

let get_data () =
  let file = !Benl_clflags.cache_dir // "monitor.cache" in
  if !use_cache && Sys.file_exists file then
    filter_affected (Marshal.load file)
  else
    let src_raw = parse_sources M.empty in
    let bin_raw = List.fold_left
      parse_binaries PAMap.empty !Benl_clflags.architectures
    in
    let data = { src_map = src_raw; bin_map = bin_raw; } in
    Marshal.dump file data;
    filter_affected data

let print_dep_line src deps =
  printf "%s:" !!!src;
  S.iter (fun dep -> printf " %s" !!!dep) deps;
  printf "\n%!"

let print_dep_graph x = M.iter print_dep_line x

let rec parse_local_args = function
  | "--use-cache"::xs ->
      use_cache := true;
      parse_local_args xs
  | "--run-debcheck"::xs ->
      run_debcheck := true;
      parse_local_args xs
  | "--color"::xs ->
      use_colors := true;
      output_type := Text;
      parse_local_args xs
  | "--text"::xs ->
      output_type := Text;
      parse_local_args xs
  | "--html"::xs ->
      output_type := Xhtml;
      parse_local_args xs
  | x::xs -> x::(parse_local_args xs)
  | [] -> []

let help () =
  List.iter
    (fun (option , desc) ->
      Printf.printf "    %s: %s\n%!" option desc
    )
    [ "--use-cache", "Use cache";
      "--run-debcheck", "Run edos-debcheck and add virtual .edos-debcheck field";
      "--color", "Color if text output";
      "--text", "Select text output format";
      "--html", "Select HTML output format" ]

let relevant_arch arch_ref arch_pkg =
  (arch_pkg = "all" && arch_ref = "i386") || arch_ref = arch_pkg

let compute_state pkg =
  if Query.eval_binary pkg !!(is_bad ()) then
    Outdated
  else if Query.eval_binary pkg !!(is_good ()) then
    Up_to_date
  else
    Unknown

let combine_states state1 state2 =
  match state1, state2 with
    | Outdated, _
    | _, Outdated -> Outdated
    | Up_to_date, _
    | _, Up_to_date -> Up_to_date
    | Unknown, Unknown -> state2

let compute_monitor_data sources binaries rounds =
  List.map begin fun xs ->
    let packages = List.sort (fun x y -> compare !!!x !!!y) xs in
    List.map begin fun src ->
      let states =
        List.map begin fun arch ->
          let pkgs = Package.binaries (M.find src sources) in
          List.fold_left begin fun accu pkg ->
            try
              let pkg = PAMap.find (pkg, arch) binaries in
              let state = compute_state pkg in
              combine_states accu state
            with Not_found -> accu
          end Unknown pkgs
        end !Benl_clflags.architectures
      in src, states
    end packages
  end rounds

let print_text_monitor sources binaries rounds =
  let monitor_data = compute_monitor_data sources binaries rounds in
  let nmax = M.fold begin fun src _ accu ->
    let n = String.length !!!src in
    if n > accu then n else accu
  end sources 0 in
  let src_fmt = Scanf.format_from_string (sprintf "%%%ds:" (nmax+2)) "%s" in
  let width =
    String.length (String.concat "   " !Benl_clflags.architectures)+6+nmax
  in
  let nrounds = String.length (string_of_int (List.length rounds)) in
  let hwidth = String.length "> Dependency level  <" + nrounds in
  let header_fmt =
    let width = width-hwidth+2 in
    let left = width/2 in
    let right = width-left in
    let buf = Buffer.create 64 in
    for i = 1 to left do Buffer.add_char buf '=' done;
    bprintf buf "> Dependency level %%%dd <" nrounds;
    for i = 1 to right do Buffer.add_char buf '=' done;
    Buffer.add_char buf '\n';
    Scanf.format_from_string (Buffer.contents buf) "%d"
  in
  list_iteri begin fun i xs ->
    printf header_fmt i;
    List.iter begin fun (src, states) ->
      printf src_fmt !!!src;
      List.iter begin fun (arch, state) ->
        printf " %s" (format_arch state arch)
      end (List.combine !Benl_clflags.architectures states);
      printf "\n";
    end xs;
    printf "\n"
  end monitor_data

let a_link url text =
  a ~a:[a_href (uri_of_string url)] [pcdata text]

let escape = Netencoding.Url.encode

let pts src =
  a_link (sprintf "http://packages.qa.debian.org/%s" (escape src)) src

let buildd show src =
  if show
  then a_link
    (sprintf "https://buildd.debian.org/status/package.php?p=%s" src)
    "buildd"
  else small [ pcdata "arch:all" ]

let changelog src dir =
  small [ a_link
    (sprintf "http://packages.debian.org/changelogs/%s/current/changelog" dir)
    src ]

module SS = Set.Make(String)

let uniq l =
  let s = List.fold_left
    begin fun s state -> SS.add (Benl_base.class_of_status state) s end
    (SS.empty) l in
  SS.elements s

let overrall_state l =
  match uniq l with
    | _ as l when List.for_all (fun s -> s = "unknown") l -> [ "unknown" ]
    | _ as l when List.for_all (fun s -> s <> "bad") l -> [ "good" ]
    | _ -> [ "bad" ]

let generate_stats monitor_data =
  List.fold_left
    (fun (all, bad, packages) level ->
      List.fold_left
        (fun (all, bad, packages) (package, statuses) ->
          if List.mem Outdated statuses then
            all+1, bad+1, package::packages
          else if List.mem Up_to_date statuses then
            all+1, bad, package::packages
          else
            all, bad, package::packages
        )
        (all, bad, packages)
        level
    )
    (0, 0, [])
    monitor_data

let starts_with text head =
  let s = try String.sub text 0 (String.length head) with _ -> text in
  s = head

let cut_head text head =
  try
    let len = String.length head in
    String.sub text len (String.length text - len)
  with _ -> text

let beautify_text =
  let r_link = Pcre.regexp "#[0-9]{4,}|[a-z]{3,}://[^\\s><]+|[Pp][Tt][Ss]:[a-z0-9+\\-\\.]+|[Bb][Uu][Ii][Ll][Dd][Dd]:[a-z0-9+\\-\\.]+" in
  fun text ->
    let t = Pcre.full_split ~rex:r_link text in
    List.map
      (function
        | Pcre.Text s -> pcdata s
        | Pcre.Delim s ->
          let l = String.lowercase s in
          if s.[0] = '#' then
            let ss = String.sub s 1 (String.length s -1) in
            let link = sprintf "http://bugs.debian.org/%s" ss in
            a_link link s
          else if starts_with l "pts" then
            let text = cut_head s "pts:" in
            let link = sprintf "http://packages.qa.debian.org/%s" text in
            a_link link s
          else if starts_with l "buildd" then
            let text = cut_head s "buildd:" in
            let link = sprintf "https://buildd.debian.org/%s" text in
            a_link link s
          else
            a_link s s
        | Pcre.Group _ | Pcre.NoGroup -> (* Ignore this case *) pcdata ""
      )
      t

let print_html_monitor sources binaries dep_graph rounds =
  let monitor_data = compute_monitor_data sources binaries rounds in
  let all, bad, packages = generate_stats monitor_data in
  let affected = List.map fst (List.flatten monitor_data) in
  let mytitle =
    try
      Query.to_string ~escape:false (Query.of_expr (Benl_clflags.get_config "title"))
    with _ -> "(no title)" in
  let notes =
    try Query.to_string ~escape:false (Query.of_expr (Benl_clflags.get_config "notes"))
    with _ -> "" in
  let is_affected = Query.to_string (Lazy.force (is_affected ())) in
  let is_good = Query.to_string (Lazy.force (is_good ())) in
  let is_bad = Query.to_string (Lazy.force (is_bad ())) in
  let archs_count = List.length !Benl_clflags.architectures in
  let html hbody =
    html ~a:[a_xmlns `W3_org_1999_xhtml]
      (head (title (pcdata (sprintf "Transition: %s" mytitle))) [
        script
          ~contenttype:"text/javascript"
          (pcdata (sprintf
                    "var nb_columns = %d; var nb_rounds = %d;"
                    (2 + archs_count)
                    (List.length monitor_data))
          );
        script
          ~contenttype:"text/javascript"
          ~a:[a_src (uri_of_string "http://code.jquery.com/jquery-latest.js")]
          (pcdata "");
        script
          ~contenttype:"text/javascript"
          ~a:[a_src (uri_of_string ("media/script.js"))]
          (pcdata "");
        link
          ~a:[a_rel [`Stylesheet];
              a_href (uri_of_string ("media/revamp.css"))
             ]
          ();
        link
          ~a:[a_rel [`Stylesheet];
              a_href (uri_of_string ("media/styles.css"))
             ]
          ();
        meta
          ~content:"text/html;charset=utf-8"
          ~a:[a_http_equiv "Content-Type"]
          ();
      ])
      (body [
        h1 ~a:[a_id "title"] [pcdata "Debian Release Management"];
        h2 ~a:[a_id "subtitle"] [pcdata (sprintf "Transition: %s" mytitle)];
        div ~a:[a_id "body"] [
          b [ pcdata "Parameters:" ];
          ul~a:[ a_class ["parameters"] ]
            (li [ small [ b [ pcdata "Affected: " ]; pcdata is_affected ] ])
            [li [ small [ b [ pcdata "Good: " ]; pcdata is_good ] ];
             li [ small [ b [ pcdata "Bad: " ]; pcdata is_bad ] ];
            ];
          if String.length notes = 0 then
            div [ ]
          else
            div ~a:[ a_class ["parameters"] ]
              [ small [ b [ pcdata "Notes: " ] ];
                pre ( beautify_text notes ) ]
          ;
          div
            [
              pcdata "Filter by status: ";
              input ~a:[a_input_type `Checkbox; a_id "good"] ();
              pcdata "good ";
              input ~a:[a_input_type `Checkbox; a_checked `Checked; a_id "bad"] ();
              pcdata "bad ";
              input ~a:[a_input_type `Checkbox; a_checked `Checked; a_id "unknown"] (); pcdata "unknown";
              span ~a:[a_id "count"] [];
            ];
          hbody;
        ];
        div ~a:[a_id "footer"] [
          small [ pcdata (sprintf "Page generated on %s" (Benl_core.get_rfc2822_date ())) ]
        ]
      ]) in
  let abrege = function
    | "hurd-i386" -> "hurd"
    | "kfreebsd-amd64" -> "kbsd64"
    | "kfreebsd-i386" -> "kbsd32"
    | "powerpc" -> "ppc"
    | x -> x in
  let archs_columns = List.map begin fun arch ->
    th [ small [ pcdata (abrege arch) ] ]
  end !Benl_clflags.architectures in
  let empty_col = td [ pcdata "" ] in
  let archs_columns round header =
    tr ~a:[ a_id (sprintf "header%d" round) ]
      header
      (empty_col :: archs_columns) in
  let rows, _ =
    List.fold_left begin fun (rows, i) xs ->
      let names, rows =
      (List.fold_left begin fun (arch_any_s, acc) (src, states) ->
        let classes = [ "src"; sprintf "round%d" i ] in
        let deps = S.elements (Package.Map.find src dep_graph) in
        let deps = List.filter (fun p -> List.mem p affected) deps in
        let deps = match (List.map (!!!) deps) with
          | [] -> ""
          | _ as l-> "Dependencies: " ^ (String.concat ", " l) in
        let source = M.find src sources in
        let version = Package.get "version" source in
        let directory = Package.get "directory" source in
        let arch_any = Package.get "architecture" source <> "all" in
        let arch_any_s = if arch_any then !!!src::arch_any_s else arch_any_s in
        let overrall_state = overrall_state states in
        let src = !!!src in
        arch_any_s,
        tr (td ~a:[ a_class ("srcname" :: (overrall_state @ classes));
                    a_id src;
                    a_title deps
                  ]
              [ pts src ])
          (
          td
            ~a:[ a_class [ "src"] ]
            [ pcdata "[";
              buildd arch_any (escape src);
              pcdata "] (";
              changelog (sprintf "%s" version) directory;
              pcdata ")" ]
          ::
          (List.map begin fun state ->
            (td ~a:[ a_class [ Benl_base.class_of_status state ] ]
               [ small [ pcdata (Benl_base.string_of_status state) ] ])
          end states)
          )
        :: acc
      end ([], rows) (List.rev xs)) in
      let link =
        if names = []
        then small [ pcdata "arch:all" ]
        else buildd true (sprintf "%s&compact=compact" (String.concat "," (List.map escape names))) in
      archs_columns i
        (th ~a:[ a_class [ "level" ] ]
           [ pcdata (sprintf "Dependency level %d" (i+1));
             pcdata " (";
             link;
             pcdata ")"
           ]
        )
      :: rows, (i - 1)
    end ([], (List.length monitor_data - 1)) (List.rev monitor_data) in
  let table = table (tr (td [ pcdata "" ]) []) rows in
  (all, bad, packages, Xhtmlpretty.xhtml_print (html table))

let print_dependency_levels dep_graph rounds =
  list_iteri begin fun i xs ->
    printf "===[ Dependency level %d ]=====\n" i;
    let packages = List.sort (fun x y -> compare !!!x !!!y) xs in
    List.iter begin fun src ->
      let deps = M.find src dep_graph in
      print_dep_line src deps
    end packages
  end rounds

let compute_graph () =
  let {src_map = sources; bin_map = binaries} = get_data () in
  let src_of_bin : ([`binary], [`source] Package.Name.t) M.t =
    PAMap.fold
      (fun (name, _) pkg accu ->
         let source = Package.get "source" pkg in
         M.add name (Package.Name.of_string source) accu)
      binaries
      M.empty
  in
  let src_of_bin =
    M.fold
      (fun name pkg accu ->
         List.fold_left
           (fun accu bin -> M.add bin name accu)
           accu
           (Package.binaries pkg))
      sources
      src_of_bin
  in
  let dep_graph = Dependencies.get_dep_graph sources src_of_bin in
  let rounds = Dependencies.topo_split dep_graph in
  rounds, sources, binaries, dep_graph

let main args =
  let _ = parse_local_args (Benl_frontend.parse_common_args args) in
  let rounds, sources, binaries, dep_graph =
    compute_graph () in
  match !output_type with
    | Levels -> print_dependency_levels dep_graph rounds
    | Text -> print_text_monitor sources binaries rounds
    | Xhtml ->
      let (_, _, _, output) =
        print_html_monitor sources binaries dep_graph rounds
      in printf "%s\n" output

let frontend = {
  Benl_frontend.name = "monitor";
  Benl_frontend.main = main;
  Benl_frontend.help = help;
}
