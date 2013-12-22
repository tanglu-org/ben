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
open Xhtml.M

module M = Package.Map
module S = Package.Set

let use_colors = ref false
let output_file = ref None
let baseurl = ref "file:///.."

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

open Benl_modules
open Marshallable
open Benl_data

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

let ben_webpage = "http://ben.debian.net"

let print_dep_line src deps =
  printf "%s:" !!!src;
  S.iter (fun dep -> printf " %s" !!!dep) deps;
  printf "\n%!"

let print_dep_graph x = M.iter print_dep_line x

let rec parse_local_args = function
  | "--run-debcheck"::xs ->
      Benl_data.run_debcheck := true;
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
  | "--use-projectb"::xs ->
      Benl_data.use_projectb := true;
      parse_local_args xs
  | ("--output"|"-o")::filename::xs ->
      output_file := Some filename;
      parse_local_args xs
  | "--template"::template::xs ->
      Benl_templates.load_template template;
      parse_local_args xs
  | x::xs -> x::(parse_local_args xs)
  | [] -> []

let help () =
  List.iter
    (fun (option , desc) ->
      Printf.printf "    %s: %s\n%!" option desc
    )
    [ "--run-debcheck", "Run dose-debcheck and add virtual .uninstallable field";
      "--use-projectb", "Get package lists from Projectb database";
      "--color", "Color if text output";
      "--text", "Select text output format";
      "--html", "Select HTML output format";
      "--output|-o", "Select output file";
      "--template", "Select an HTML template";
    ]

let check_media_dir base =
  let mediad = base // "media" in
  if not (Sys.file_exists mediad) then
    Unix.symlink !Benl_clflags.media_dir mediad
  else
    match (Unix.stat mediad).Unix.st_kind with
    | Unix.S_LNK ->
        let target = Unix.readlink mediad in
        if target != !Benl_clflags.media_dir then begin
          Unix.unlink mediad;
          Unix.symlink !Benl_clflags.media_dir mediad
        end
    | _ -> ()

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
    List.map begin fun sname ->
      let src = M.find sname sources in
      let src_name = Package.get "package" src in
      let states =
        List.map begin fun arch ->
          (* FIXME: indexing by name+arch is not a good idea after all *)
          arch, PAMap.fold (fun (_, arch') pkg accu ->
            if arch' = arch &&
              Package.get "source" pkg = src_name
            then
              let state = compute_state pkg in
              combine_states accu state
            else
              accu
          ) binaries Unknown
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
      let in_testing =
        try not (Package.get "is-in-testing" src = "no")
        with Not_found -> true
      in
      let sname = Package.get "package" src in
      let sname = if in_testing then sname else "_"^sname in
      printf "%*s:" (nmax+3) sname;
      List.iter begin fun (arch, state) ->
        printf " %s" (format_arch state arch)
      end states;
      printf "\n";
    end xs;
    printf "\n"
  end monitor_data

let a_link url text =
  a ~a:[a_href (uri_of_string url)] [pcdata text]

let escape = Netencoding.Url.encode

let pts t src =
  a_link (t.Template.pts (escape src)) src

let buildd t show src ver =
  if show
  then a_link (t.Template.buildd src ver) "build logs"
  else small [ pcdata "arch:all" ]

let buildds t show srcs =
  match t.Template.buildds srcs with
  | None -> raise Exit
  | Some s -> a_link s "build logs"

let rc_bugs t src =
  match t.Template.critical_bugs src with
  | None -> raise Exit
  | Some s -> a_link s "RC bugs"

let changelog t ver dir src =
  small [ a_link ( t.Template.changelog dir src ver) ver ]

let generated_on_text () =
  [ pcdata "Page generated by ";
    a_link ben_webpage "Ben";
    pcdata (Printf.sprintf " on %s" (Benl_core.get_rfc2822_date ()))
  ]

module SS = Set.Make(String)

let overrall_state l =
  let _ (* ignored_archs *), release_archs =
    List.partition
      (fun (arch,_) -> List.mem arch !Benl_base.ignored_architectures)
      l in
  if List.for_all (fun (_,status) -> status = Unknown) release_archs then
    Unknown
  else if List.for_all (fun (_,status) -> status <> Outdated) release_archs then
    Up_to_date
  else
    Outdated

let generate_stats monitor_data =
  List.fold_left
    (fun (all, bad, packages) level ->
      List.fold_left
        (fun (all, bad, packages) (package, statuses) ->
          let is_in_testing =
            try Package.get "is-in-testing" package = "yes"
            with _ -> false
          in
          let package =
            Package.Name.of_string (Package.get "package" package)
          in
          let overrall_state = overrall_state statuses in
          let packages = package::packages in
          let return all bad =
            all, bad, packages
          in
          match overrall_state with
          | Outdated when (not !Benl_data.use_projectb || is_in_testing) ->
              return (all+1) (bad+1)
          | Up_to_date ->
              return (all+1) bad
          | _ ->
              return all bad
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
  let r_link = Pcre.regexp "#[0-9]{4,}|[a-z]{3,}://[^\\s><]+|<[^\\s><]+@[^\\s><]+>|[Pp][Tt][Ss]:[a-z0-9+\\-\\.]+|[Bb][Uu][Ii][Ll][Dd][Dd]:[a-z0-9+\\-\\.]+" in
  fun tpl text ->
    let t = Pcre.full_split ~rex:r_link text in
    List.map
      (function
        | Pcre.Text s -> pcdata s
        | Pcre.Delim s ->
          let l = String.lowercase s in
          if s.[0] = '#' then
            let ss = String.sub s 1 (String.length s -1) in
            let link = tpl.Template.bugs ss in
            a_link link s
          else if s.[0] = '<' then
            let ss = String.sub s 1 (String.length s - 2) in
            let link = tpl.Template.msg_id ss in
            a_link link s
          else if starts_with l "pts" then
            let text = cut_head s "pts:" in
            let link = tpl.Template.pts text in
            a_link link s
          else if starts_with l "buildd" then
            let text = cut_head s "buildd:" in
            let link = tpl.Template.buildd text "" in
            a_link link s
          else
            a_link s s
        | Pcre.Group _ | Pcre.NoGroup -> (* Ignore this case *) pcdata ""
      )
      t

let print_html_monitor template sources binaries dep_graph rounds =
  let monitor_data = compute_monitor_data sources binaries rounds in
  let all, bad, packages = generate_stats monitor_data in
  let affected = List.map (fun x ->
    Package.Name.of_string (Package.get "package" (fst x))
  ) (List.flatten monitor_data) in
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
  let has_testing_data = match monitor_data with
    | ((src, _) :: _) :: _ ->
      (* if is-in-testing has been injected, it has been injected into
         all packages, so just pick any *)
      (try let _ = Package.get "is-in-testing" src in true
       with Not_found -> false)
    | _ -> false
  in
  let page_title = sprintf "Transition: %s" mytitle in
  let extra_headers = [
    script
      ~contenttype:"text/javascript"
      (pcdata (sprintf
                 "var nb_columns = %d; var nb_rounds = %d;"
                 (2 + archs_count)
                 (List.length monitor_data))
      );

    script
      ~contenttype:"text/javascript"
      ~a:[a_src (uri_of_string "media/jquery.min.js")]
      (pcdata "");

    script
      ~contenttype:"text/javascript"
      ~a:[a_src (uri_of_string ("media/script.js"))]
      (pcdata "");
  ] in
  let hbody table = [
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
          pre ( beautify_text template notes ) ]
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
    div (if has_testing_data
      then [
        input ~a:[a_input_type `Checkbox; a_id "notintesting"] ();
        pcdata "ignore packages that are not in testing";
      ]
      else []
    );
    table;
  ] in
  let footer = [ small (generated_on_text ()) ] in
  let abrege = function
    | "hurd-i386" -> "hurd"
    | "kfreebsd-amd64" -> "kbsd64"
    | "kfreebsd-i386" -> "kbsd32"
    | "powerpc" -> "ppc"
    | x -> x in
  let archs_columns = List.map begin fun arch ->
    th [ small [ pcdata (abrege arch) ] ]
  end !Benl_clflags.architectures in
  let archs_columns round header =
    tr ~a:[ a_id (sprintf "header%d" round) ]
      header archs_columns in
  let rows, _ =
    List.fold_left begin fun (rows, i) xs ->
      let names, rows =
      (List.fold_left begin fun (arch_any_s, acc) (source, states) ->
        let src = Package.get "package" source in
        let has_ma_same =
          List.exists (fun bin ->
            List.exists (fun arch ->
              try
                let pkg = PAMap.find (bin, arch) binaries in
                Package.get "multi-arch" pkg = "same"
              with Not_found -> false
            ) !Benl_clflags.architectures
          ) (Package.binaries source)
        in
        let has_ma_same_html =
          if has_ma_same then [
            pcdata " [";
            small [pcdata "ma:same"];
            pcdata "]";
          ] else []
        in
        let in_testing =
          try not (Package.get "is-in-testing" source = "no")
          with Not_found -> true
        in
        let classes =
          [ "src"; sprintf "round%d" i ] @
            (if in_testing then [] else ["notintesting"])
        in
        let deps = S.elements
          (Package.Map.find (Package.Name.of_string src) dep_graph)
        in
        let deps = List.filter (fun p -> List.mem p affected) deps in
        let deps = match (List.map (!!!) deps) with
          | [] -> ""
          | _ as l-> "Dependencies: " ^ (String.concat ", " l) in
        let version = Package.get "version" source in
        let directory = Package.get "directory" source in
        let arch_any = Package.get "architecture" source <> "all" in
        let arch_any_s = if arch_any then src::arch_any_s else arch_any_s in
        let overrall_state = [ class_of_status (overrall_state states) ] in
        let src_text =
          (pts template src)::(if in_testing then [] else [pcdata " (sid only)"])
        in
        arch_any_s,
        tr (td ~a:[ a_class ("srcname" :: (overrall_state @ classes));
                    a_id src;
                    a_title deps
                  ]
              src_text)
          (
          td
            ~a:[ a_class [ "src"] ]
            ([ pcdata "[";
              buildd template arch_any (escape src) version;
              pcdata "] (";
              changelog template (sprintf "%s" version) directory (escape src);
              pcdata ")" ] @ has_ma_same_html)
          ::
          (List.map begin fun (_, state) ->
            (td ~a:[ a_class [ Benl_base.class_of_status state ] ]
               [ small [ pcdata (Benl_base.string_of_status state) ] ])
          end states)
          )
        :: acc
      end ([], rows) (List.rev xs)) in
      let column_arg = try
        let buildd_link =
          if names = []
          then small [ pcdata "arch:all" ]
          else buildds template true (List.map escape names) in
        let rc_bugs_link = rc_bugs template (List.map escape names) in
        [
          pcdata " (";
          buildd_link; pcdata " "; rc_bugs_link;
          pcdata ")"
        ]
        with Exit -> []
      in
      archs_columns i
        (th ~a:[ a_colspan 2; a_class [ "level" ] ]
           (pcdata (sprintf "Dependency level %d" (i+1)) :: column_arg)
        )
      :: rows, (i - 1)
    end ([], (List.length monitor_data - 1)) (List.rev monitor_data) in
  let table = table (tr (td [ pcdata "" ]) []) rows in
  let subtitle =
    [a_link (Filename.concat !baseurl "index.html") "Transitions";
     pcdata (Printf.sprintf " → %s" mytitle)
    ] in
  let html = template.Template.page page_title subtitle extra_headers (hbody table) footer in
  (all, bad, packages, html)

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
  let {src_map = sources; bin_map = binaries} = get_data is_affected in
  let src_of_bin : ([`binary], [`source] Package.Name.t) M.t =
    PAMap.fold
      (fun (name, _) pkg accu ->
         let source = Package.get "source" pkg in
         M.add name (Package.Name.of_string source) accu)
      binaries
      M.empty
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
      let template = Benl_templates.get_registered_template () in
      let (_, _, _, output) =
        print_html_monitor template sources binaries dep_graph rounds
      in
      match !output_file with
      | None ->
          Xhtml.P.print print_string output;
          print_newline ()
      | Some file ->
          check_media_dir (Filename.basename file);
          Benl_utils.dump_xhtml_to_file file output

let frontend = {
  Benl_frontend.name = "monitor";
  Benl_frontend.main = main;
  Benl_frontend.help = help;
}
