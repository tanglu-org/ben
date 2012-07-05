
open Xhtml.M

let page _title _headers _body _footer =
  let _headers =
    (meta
      ~content:"text/html;charset=utf-8"
      ~a:[a_http_equiv "Content-Type"]
      ())
    ::
    (link
      ~a:[a_rel [`Stylesheet];
          a_href (uri_of_string ("media/revamp.css"))
         ]
      ())
    ::
    (link
       ~a:[a_rel [`Stylesheet];
           a_href (uri_of_string ("media/styles.css"))
          ]
       ())
    ::
    _headers in
  html ~a:[a_xmlns `W3_org_1999_xhtml]
    (head
       (title (pcdata _title))
       _headers
    )
    (body [
      h1 ~a:[a_id "title"]
        [a ~a:[a_href (uri_of_string "http://release.debian.org/")] [pcdata "Debian Release Management"]];
      h2 ~a:[a_id "subtitle"]
        [a ~a:[a_href (uri_of_string "http://release.debian.org/transitions/")] [pcdata  "Transitions"];
         pcdata (Printf.sprintf " → %s" _title)];
      div ~a:[a_id "body"] _body;
      div ~a:[a_id "footer"] _footer
    ])

let () =
  Benl_templates.register_template
    "Debian"
    page
