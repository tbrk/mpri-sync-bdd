TODO
====
* Use an installed version of the bdd library.
* Save button to download editor contents.
* Share buttom to send/receive editor contents (using gists?)
* Resizable partition between terminal and editor
  (make responsive).
* Fix syntax highlighting of code in the editor.
* Turn URLs in editor into links.
* Highlight error messages in the editor?
* Add a ppx extension to get variables from BDD session?
  I.e., to replace
    module CTR4 = (val named_variables ["c0" ; "c1" ; "c2" ; "c3" ;
                                        "c0'"; "c1'"; "c2'"; "c3'"])
    let (c0, c1, c2, c3, c0', c1', c2', c3')
      = CTR4.(x 1, x 2, x 3, x 4, x 5, x 6, x 7, x 8)

  with something like
    module%bdd CTR4 = named_variables ["c0" ; "c1" ; "c2" ; "c3" ;
                                       "c0'"; "c1'"; "c2'"; "c3'"]

