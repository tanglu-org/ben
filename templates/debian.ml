
open Xhtml.M

let href uri =
  a_href (uri_of_string uri)

let a_link url text =
  a ~a:[a_href (uri_of_string url)] [pcdata text]

let page _title _headers _body _footer =
  let _headers =
    (meta
      ~content:"text/html;charset=utf-8"
      ~a:[a_http_equiv "Content-Type"]
      ())
    ::
    (link ~a:[a_rel [`Stylesheet]; a_href "media/revamp.css"] ())
    ::
    (link ~a:[a_rel [`Stylesheet]; a_href "media/styles.css"] ())
    ::
    _headers in
  html ~a:[a_xmlns `W3_org_1999_xhtml]
    (head
       (title (pcdata _title))
       _headers
    )
    (body [
      h1 ~a:[a_id "title"]
        [a_link "http://release.debian.org/" "Debian Release Management"];
      h2 ~a:[a_id "subtitle"]
        [a_link "http://release.debian.org/transitions/" "Transitions";
         pcdata (Printf.sprintf " → %s" _title)
        ];
      div ~a:[a_id "body"] _body;
      div ~a:[a_id "footer"] _footer
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
    pts = "http://packages.qa.debian.org/%s";
    changelog = "http://packages.debian.org/changelogs/%s/%s_%s/changelog";
    buildd = "https://buildd.debian.org/status/package.php?p=%s&amp;ver=%s";
    buildds = Some "https://buildd.debian.org/status/package.php?p=%s&amp;compact=compact";
    bugs = "http://bugs.debian.org/%s";
    critical_bugs = Some "http://bugs.debian.org/cgi-bin/pkgreport.cgi?sev-inc=serious;sev-inc=grave;sev-inc=critical;src=%s";
    msg_id = "http://lists.debian.org/%s";
  }
