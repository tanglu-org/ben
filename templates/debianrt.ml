open Printf
open Xhtml.M

let href uri =
  a_href (uri_of_string uri)

let a_link url text =
  a ~a:[a_href (uri_of_string url)] [pcdata text]

let page ~title ~subtitle ~headers ~body ~footer =
  let headers =
    (meta
      ~content:"text/html;charset=utf-8"
      ~a:[a_http_equiv "Content-Type"]
      ())
    ::
    (link ~a:[a_rel [`Stylesheet]; a_href "media/revamp.css"] ())
    ::
    (link ~a:[a_rel [`Stylesheet]; a_href "media/styles.css"] ())
    ::
    headers in
  html ~a:[a_xmlns `W3_org_1999_xhtml]
    (head
       (Xhtml.M.title (pcdata title))
       headers
    )
    (Xhtml.M.body ~a:[a_class ["debian"]] [
      h1 ~a:[a_id "title"]
        [a_link "http://release.debian.org/" "Debian Release Management"];
      h2 ~a:[a_id "subtitle"] subtitle;
      div ~a:[a_id "body"] body;
      div ~a:[a_id "footer"] footer
    ])

open Template

let () =
  Benl_templates.register_template {
    name = "Debian";
    page;
    intro = [
      b [ a_link
            "http://wiki.debian.org/Teams/ReleaseTeam/Transitions"
            "Transition documentation"
        ];
      br ();
      b [ a_link
            "http://bugs.debian.org/cgi-bin/pkgreport.cgi?users=release.debian.org@packages.debian.org;tag=transition"
            "Bugs tagged \"transition\""
        ];
      br ();
      br ();
    ];
    pts = (fun ~src -> sprintf "http://packages.qa.debian.org/%s" src);
    changelog = (fun ~letter ~src ~ver -> sprintf "http://packages.debian.org/changelog:%s" src);
    buildd = (fun ~src ~ver -> sprintf "https://buildd.debian.org/status/package.php?p=%s" src);
    buildds = (fun ~srcs ->
      let srcs = String.concat "," srcs in
      Some (sprintf "https://buildd.debian.org/status/package.php?p=%s&amp;compact=compact" srcs));
    bugs = (fun ~src -> sprintf "http://bugs.debian.org/%s" src);
    critical_bugs = (fun ~srcs ->
      let srcs = String.concat ";src=" srcs in
      Some (sprintf "http://bugs.debian.org/cgi-bin/pkgreport.cgi?sev-inc=serious;sev-inc=grave;sev-inc=critical;src=%s" srcs));
    msg_id = (fun ~mid -> sprintf "http://lists.debian.org/%s" mid);
  }
