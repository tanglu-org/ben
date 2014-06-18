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
    (Xhtml.M.body ~a:[a_class ["tanglu"]] [
      h1 ~a:[a_id "title"]
        [a_link "http://qa.tanglu.org/transitions/" "Tanglu QA"];
      h2 ~a:[a_id "subtitle"] subtitle;
      div ~a:[a_id "body"] body;
      div ~a:[a_id "footer"] footer
    ])

open Template

let () =
  Benl_templates.register_template {
    name = "Tanglu";
    page;
    intro = [
      b [ a_link
            "http://wiki.debian.org/Teams/ReleaseTeam/Transitions"
            "Transition documentation"
        ];
      br ();
      br ();
    ];
    pts = (fun ~src -> sprintf "http://packages.tanglu.org/src:%s" src);
    changelog = (fun ~letter ~src ~ver -> sprintf "http://packages.debian.org/changelog:%s" src);
    buildd = (fun ~src ~ver -> sprintf "http://buildd.tanglu.org/source/%s/" src);
    buildds = (fun ~srcs ->
      let srcs = String.concat "," srcs in
      Some (sprintf "https://qa.tanglu.org"));
    bugs = (fun ~src -> sprintf "http://bugs.tanglu.org/buglist.cgi?product=Tanglu&component=%s&resolution=---" src);
    critical_bugs = (fun ~srcs ->
      let srcs = String.concat "&component=" srcs in
      Some (sprintf "http://bugs.tanglu.org/buglist.cgi?product=Tanglu%s&resolution=---" srcs));
    msg_id = (fun ~mid -> sprintf "http://lists.tanglu.org/%s" mid);
  }
