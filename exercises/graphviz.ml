(* Graphviz examples from https://graphs.grevian.org/example *)

let _ = gv ~engine:"circo"
    "graph { a -- b; b -- c; c -- d; d -- e; e -- f; a -- f; a -- c;
             a -- d; a -- e; b -- d; b -- e; b -- f; c -- e; c -- f;
             d -- f; }"

let _ = gv ~engine:"circo"
  "digraph {
      a -> b[label=\"0.2\",weight=\"0.2\"];
      a -> c[label=\"0.4\",weight=\"0.4\"];
      c -> b[label=\"0.6\",weight=\"0.6\"];
      c -> e[label=\"0.6\",weight=\"0.6\"];
      e -> e[label=\"0.1\",weight=\"0.1\"];
      e -> b[label=\"0.7\",weight=\"0.7\"];
  }"

let _ = gv "graph {
                a -- b[color=red,penwidth=3.0];
                b -- c;
                c -- d[color=red,penwidth=3.0];
                d -- e;
                e -- f;
                a -- d;
                b -- d[color=red,penwidth=3.0];
                c -- f[color=red,penwidth=3.0];
            }"

let _ = gv "digraph {
              subgraph cluster_0 {
                  label=\"Subgraph A\";
                  a -> b;
                  b -> c;
                  c -> d;
              }
              subgraph cluster_1 {
                  label=\"Subgraph B\";
                  a -> f;
                  f -> c;
              }
          }"

let _ = gv "graph {
                rankdir=LR; // Left to Right, instead of Top to Bottom
                a -- { b c d };
                b -- { c e };
                c -- { e f };
                d -- { f g };
                e -- h;
                f -- { h i j g };
                g -- k;
                h -- { o l };
                i -- { l m j };
                j -- { m n k };
                k -- { n r };
                l -- { o m };
                m -- { o p n };
                n -- { q r };
                o -- { s p };
                p -- { s t q };
                q -- { t r };
                r -- t;
                s -- z;
                t -- z;
            }"

let _ = gv "graph {
              rankdir=LR;
              a -- { b c d }; b -- { c e }; c -- { e f }; d -- { f g }; e -- h;
              f -- { h i j g }; g -- k; h -- { o l }; i -- { l m j }; j -- { m n k };
              k -- { n r }; l -- { o m }; m -- { o p n }; n -- { q r };
              o -- { s p }; p -- { s t q }; q -- { t r }; r -- t; s -- z; t -- z;
              { rank=same; b, c, d }
              { rank=same; e, f, g }
              { rank=same; h, i, j, k }
              { rank=same; l, m, n }
              { rank=same; o, p, q, r }
              { rank=same; s, t }
          }"

