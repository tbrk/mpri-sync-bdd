(* Cours MPRI Parallélisme synchrone
   Basic exercises on model checking with BDDs.

   Based on P. Raymond's paper on
   “Synchronous program verification with Lustre/Lesar”
   (https://www.di.ens.fr/~pouzet/cours/mpri/cours8/lesar-rapport.pdf)

   And J.C. Filliâtre's BDD library
   (https://github.com/tbrk/jcf-bdd)

   See also: H. Reif Anderson's
   “An Introduction to Binary Decision Diagrams”
   (http://www.cs.utexas.edu/~isil/cs389L/bdd.pdf)
*)

(* #load "bdd.cmo";; (* not needed for web interface *) *)

open Bdd

module type ENRICHED_BDD
= sig
    include BDD
    val x       : variable -> t
    val (&&)    : t -> t -> t
    val not     : t -> t
    val (||)    : t -> t -> t
    val (><)    : t -> t -> t
    val (==>)   : t -> t -> t
    val (==)    : t -> t -> t
    val ite     : t -> t -> t -> t
    val gv      : ?title:string -> t -> unit
    val forall  : variable -> t -> t
    val foralls : variable list -> t -> t
    val exists  : variable -> t -> t
    val existss : variable list -> t -> t
    val subst   : t -> variable * t -> t
    val substs  : t -> (variable * t) list -> t
  end
module Enrich (B : BDD) : ENRICHED_BDD
= struct
    include B
    let x     = mk_var
    let (&&)  = mk_and
    let not   = mk_not
    let (||)  = mk_or
    let (><)  = apply (<>)
    let (==>) = mk_imp
    let (==)  = apply (=)
    let ite x y z = (x && y) || (not x && z)
    (* let gv ?title b = gv ?title (to_dot b) (* online version *) *)
    (* let gv ?title b = display b (* offline version *) *)
    let gv ?title b = (* offline mac version *)
       let file = Filename.temp_file "bdd" ".dot" in
       let ofile = Filename.temp_file "bdd" ".ps" in
       print_to_dot b ~file;
       begin try
         ignore (Sys.command (Printf.sprintf "dot -Tps %s > %s" file ofile));
         ignore (Sys.command (Printf.sprintf "open %s" ofile))
       with _ -> ()
       end;
       try
         Sys.remove file;
         Sys.remove ofile
       with _ -> ()
    let forall x t = failwith "not implemented"
    let foralls xs t = failwith "not implemented"
    let exists x t = failwith "not implemented"
    let existss xs t = failwith "not implemented"
    let subst t (x, t') = failwith "not implemented"
    let substs = failwith "not implemented"
end

(* * Useful functions *)

(* BDDs with named variables *)
let named_variables (vars : string list) =
  let nv = List.length vars in
  let v2n = Array.make nv "" in
  List.iteri (Array.set v2n) vars;
  let print_var ff x = Format.pp_print_string ff (v2n.(x - 1)) in
  (module Enrich (val make ~print_var nv) : ENRICHED_BDD)

(* Show a satisfying assignment. *)
let show_sat print_var =
  let open Format in
  let show_var ff (x, b) =
    fprintf ff "%s%a" (if b then "" else "!") print_var x in
  printf "%a\n@."
    (pp_print_list ~pp_sep:(fun ff () -> pp_print_string ff " ") show_var)

(* Show satisfying assignments. *)
let show_sats print_var =
  let open Format in
  let show_var ff (x, b) =
    fprintf ff "%s%a" (if b then "" else "!") print_var x in
  let show_sat ff s =
    pp_print_string ff " -- ";
    pp_print_list ~pp_sep:(fun ff () -> pp_print_string ff " ") show_var ff s
  in
  printf "[\n%a\n]@."
    (pp_print_list ~pp_sep:(fun ff () -> pp_print_string ff "\n") show_sat)

(* Raymond §7.4: Basic BDDs *)

module B = Enrich (val Bdd.make 6)

let (x1 : B.t) = B.mk_var (1 : variable) (* BDDs from variables *)
let x2 = B.x 2 (* abbreviation: x = mk_var *)
let x3 = B.x 3

let _ = B.(gv x1)   (* Show the BDD for "x1" *)

let _ = B.(gv ~title:"true"  one)
let _ = B.(gv ~title:"false" zero)

let b1 = B.((x1 && x2) >< x3)

let _ = B.gv ~title:"b1" b1
let _ = show_sats B.print_var (B.all_sat b1)

let _ = B.(gv ~title:"not b1" (not b1))

module C = (val named_variables ["a"; "b"; "c"])
let (a, b, c) = C.(mk_var 1, mk_var 2, mk_var 3)

let b2 = C.(a && (b ==> c))

let _ = C.gv ~title:"a ∧ (b ⇒ C)" b2

(* Raymond §7.6.3: Cofactors *)

let _ = C.(gv ~title:"restrict b2 'b true" (restrict b2 (var b) true))

let _ = C.(gv ~title:"restrict b2 'b false" (restrict b2 (var b) false))

(* Raymond §7.3.1: Shannon decomposition *)
let ok = C.(tautology (b2 ==
  (b && (restrict b2 (var b) true) || (b && (restrict b2 (var b) false))))
)

(** Q1. Fix the formula above. *)
(* TODO *)

(** Q2. Implement more BDD operations.
    Raymond §7.6.3: Quantifiers
    Reif Anderson §4.7: Existential Quantification and Substitution

    Extend the Enriched BDD functor with the following operations.

    * exists (x : variable) (b : t)
      returns ∃x, b

    * exists [x1; ...; xn] b
      returns ∃x1 ... xn, b

    * subst (b : t) ((x, bx) : variable * t)
      returns b[bx/x] (in b, substitute bx for x)

    * subst b [(x1, b1); ...; (xn, bn)]
      returns ((b[b1/x1])...)[bn/xn]
 *)
(* TODO *)

let b3 = B.(forall 2 b1)
let _ = B.gv ~title:"∀x2, b1" b3

let b4 = B.(foralls [1; 2; 3] b1)
let _ = B.gv ~title:"∀x1, x2, x3, b1" b4

let b5 = B.(exists 2 b1)
let _ = B.gv ~title:"∃x2, b1" b5

let b6 = B.(existss [1; 2; 3] b1)
let _ = B.gv ~title:"∃x1, x2, x3, b1" b6

let b7 = B.(subst b1 (3, x 4 && x 5))
let _ = B.gv b7

let _ = show_sat B.print_var (B.any_sat b1)
let ok = B.(tautology (substs b1 [(var x3, one); (var x1, zero)]))

(* Raymond §7.7: Effect of variable ordering: k = 3, n = 2k *)

module VO1 = (val named_variables ["v1"; "v2"; "v3"; "v4"; "v5"; "v6"])
let (v1, v2, v3, v4, v5, v6) = VO1.(x 1, x 2, x 3, x 4, x 5, x 6)

let e1 = VO1.(List.fold_left mk_and one [
    v1 >< v4;
    v2 >< v5;
    v3 >< v6
  ])
let _ = VO1.gv ~title:"order: v1 < v2 < v3 < v4 < v5 < v6" e1

module VO2 = (val named_variables ["v1"; "v4"; "v2"; "v5"; "v3"; "v6"])
let (v1, v4, v2, v5, v3, v6) = VO2.(x 1, x 2, x 3, x 4, x 5, x 6)

let e2 = VO2.(List.fold_left mk_and one [
    v1 >< v4;
    v2 >< v5;
    v3 >< v6
  ])
let _ = VO2.gv ~title:"order: v1 < v4 < v2 < v5 < v3 < v6" e2

(* Raymond §7.9.2: Generalized cofactor *)

let e1 = C.(b && c)

let e2 = C.(a || c)

let e = C.(constrain e1 e2)

let _ = C.gv ~title:"b ∧ c" e1
let _ = C.gv ~title:"a ∨ c" e2
let _ = C.gv ~title:"e1 ↓↓ e2" e

let e3 = C.(constrain e1 b)
let e4 = C.(constrain e1 (not b))

(* Raymond §7.9.3: Restriction *)

let e5 = C.(restriction e1 b)
let _ = C.gv ~title:"e1 ↓ b" e5

(* Raymond §8: Forward Symbolic Exploration *)

(* Q4. Implement a simple counter model with four states:

        ↓
        c0 → c1 → c2 → c3
        ↑               |
        +---------------+
 *)
(* TODO *)

(* Raymond §8.1: General scheme *)

module FORWARD1 (B : ENRICHED_BDD) = struct
  (** Q3. Implement the scheme described in Figure 15.
          (The Figure is missing something...) *)
  let is_reachable
    ?(show=false)
    (s_init : B.t)
    (post_h : B.t -> B.t)
    (state_subst : (variable * B.t) list)
    (err : B.t) =
    failwith "not implemented" (* TODO *)
end

(* Q4. Confirm that state c3 is reachable using your answer to Q3. *)
(* TODO *)


(* Raymond §8.2: Detailed implementation *)
module FORWARD2 (B : ENRICHED_BDD) = struct
  (** Q5. Implement the optimization proposed in Figure 17.
          (The Figure contains two errors...) *)
  let is_reachable
    ?(show=false)
    (s_init : B.t)
    (post_h : B.t -> B.t)
    (state_subst : (variable * B.t) list)
    (err : B.t) =
    failwith "not implemented" (* TODO *)
end

(* Q6. Apply the scheme above to test reachability of the simple counter. *)
(* TODO *)

(* Going further: implement the optimized iamge computation described
   in Raymdon §8.4. *)

(* Transforming (Boolean) Lustre programs into transition systems. *)

(* Going further: encode the following program using BDDs.

  node switch (orig, on, off : bool) returns (s : bool);
  let
    s = orig -> pre (if s then not off else on);
  tel
*)


