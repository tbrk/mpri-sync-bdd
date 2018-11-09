OCaml Toplevel with BDDs for MPRI Parallélisme synchrone
========================================================

An interactive top-level for following the BDD exercises of the MPRI course 
on Synchronous Parallelism.

This work builds on several key technologies:

* [OCaml](https://ocaml.org/) (of course)
* [js_of_ocaml](https://ocsigen.org/js_of_ocaml/)
* [Learn OCaml](https://try.ocamlpro.com/learn-ocaml-demo/)
* [ACE Editor](https://ace.c9.io)
* [svg-pan-zoom](https://github.com/ariutta/svg-pan-zoom)
* [Graphviz](https://www.graphviz.org)
* [viz.js](http://viz-js.com)

The exercises build on the libraries:

* [J.-C. Filliâtre](https://www.lri.fr/~filliatr/)'s
  [BDD library](https://www.lri.fr/~filliatr/software.en.html).

More details and the license information can be found in `about.html`.

New directives
--------------
```
#exercises
#load_exercise "<name>"
```

Dependencies
------------
```
opam install ocamlfind \
             higlo \
             ocp-indent \
             js_of_ocaml \
             js_of_ocaml-ppx \
             js_of_ocaml-tyxml \
             js_of_ocaml-toplevel \
             js_of_ocaml-lwt \
```

Building
--------
```
make
```

