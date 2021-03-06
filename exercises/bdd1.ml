(* Cours INFO-M1-REAC:
   Modèles et langages pour la programmation des systèmes réactifs

   Basic exercises on model checking with BDDs.

   Based on P. Raymond's article on
   “Synchronous program verification with Lustre/Lesar” (Nov. 2018 revision)
   (https://www.di.ens.fr/~pouzet/cours/mpri/bib/lesar-rapport.pdf)

   And J.C. Filliâtre's BDD library
   (https://github.com/backtracking/ocaml-bdd)

   See also: H. Reif Anderson's
   “An Introduction to Binary Decision Diagrams”
   (http://www.cs.utexas.edu/~isil/cs389L/bdd.pdf)
*)

(*
#require "bdd.cmo";; (* not needed for web interface *)
*)

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
    val show    : ?title:string -> t -> unit
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
    let show ?title b = gv ?title (to_dot b) (* online version *)
    (* let show ?title b = display b (* offline version *) *)
    let forall x t = failwith "not implemented"
    let foralls xs t = failwith "not implemented"
    let exists x t = failwith "not implemented"
    let existss xs t = failwith "not implemented"
    let subst t (x, t') = failwith "not implemented"
    let substs xs = failwith "not implemented"
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

(* See the definitions at
   https://github.com/backtracking/ocaml-bdd/blob/master/lib/bdd.ml *)

module B = Enrich (val Bdd.make 6)

let (x1 : B.t) = B.mk_var (1 : variable) (* BDDs from variables *)
let x2 = B.x 2 (* abbreviation: x = mk_var *)
let x3 = B.x 3

let _ = B.(show x1)   (* Show the BDD for "x1" *)

let _ = B.(show ~title:"true"  one)

let _ = B.(show ~title:"false" zero)

let b1 = B.((x1 && x2) >< x3)

let _ = B.show ~title:"b1" b1
let _ = show_sats B.print_var (B.all_sat b1)

let _ = B.(show ~title:"not b1" (not b1))

module C = (val named_variables ["a"; "b"; "c"])
let (a, b, c) = C.(mk_var 1, mk_var 2, mk_var 3)

let b2 = C.(a && (b ==> c))

let _ = C.show ~title:"a ∧ (b ⇒ C)" b2

(* Raymond §7.6.3: Cofactors *)

let _ = C.(show ~title:"restrict b2 'b true" (restrict b2 (var b) true))

let _ = C.(show ~title:"restrict b2 'b false" (restrict b2 (var b) false))

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

    * forall (x : variable) (b : t)
      returns ∀x, b

    * forall [x1; ...; xn] b
      returns ∀x1 ... xn, b

    * subst (b : t) ((x, bx) : variable * t)
      returns b[bx/x] (in b, substitute bx for x, i.e., replace x with bx)

    * subst b [(x1, b1); ...; (xn, bn)]
      returns ((b[b1/x1])...)[bn/xn]
 *)
(* TODO *)

let b3 = B.(forall 2 b1)
let _ = B.show ~title:"∀x2, b1" b3

let b4 = B.(foralls [1; 2; 3] b1)
let _ = B.show ~title:"∀x1, x2, x3, b1" b4

let b5 = B.(exists 2 b1)
let _ = B.show ~title:"∃x2, b1" b5

let b6 = B.(existss [1; 2; 3] b1)
let _ = B.show ~title:"∃x1, x2, x3, b1" b6

let b7 = B.(subst b1 (3, x 4 && x 5))
let _ = B.show b7

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
let _ = VO1.show ~title:"order: v1 < v2 < v3 < v4 < v5 < v6" e1

module VO2 = (val named_variables ["v1"; "v4"; "v2"; "v5"; "v3"; "v6"])
let (v1, v4, v2, v5, v3, v6) = VO2.(x 1, x 2, x 3, x 4, x 5, x 6)

let e2 = VO2.(List.fold_left mk_and one [
    v1 >< v4;
    v2 >< v5;
    v3 >< v6
  ])
let _ = VO2.show ~title:"order: v1 < v4 < v2 < v5 < v3 < v6" e2

(* Raymond §7.9.2: Generalized cofactor *)

let e1 = C.(b && c)

let e2 = C.(a || c)

let e = C.(constrain e1 e2)

let _ = C.show ~title:"b ∧ c" e1
let _ = C.show ~title:"a ∨ c" e2
let _ = C.show ~title:"e1 ↓↓ e2" e

let e3 = C.(constrain e1 b)
let e4 = C.(constrain e1 (not b))

(* Raymond §7.9.3: Restriction *)

let e5 = C.(restriction e1 b)
let _ = C.show ~title:"e1 ↓ b" e5

(* Raymond §8: Forward Symbolic Exploration *)

(* Q3. Implement a simple counter model with four states:

        ↓
        c0 → c1 → c2 → c3
        ↑               |
        +---------------+

       We will test that c3 is reachable by considering it to be
       an error state.
 *)

module CTR4 = (val named_variables ["c0" ; "c1" ; "c2" ; "c3" ;
                                    "c0'"; "c1'"; "c2'"; "c3'"])
let (c0, c1, c2, c3, c0', c1', c2', c3')
  = CTR4.(x 1, x 2, x 3, x 4, x 5, x 6, x 7, x 8)

let c_init  = (* TODO *)
let c_err   = c3
let c_trans = (* TODO *)
let c_post_h s = (* TODO *)
let state_subst = CTR4.([ (var c0', c0);
                          (var c1', c1);
                          (var c2', c2);
                          (var c3', c3) ])

(* Q4. Manually calculate the reachable states s_n after n transitions. *)

let s0 = c_init
let s1 = (* TODO *)
let s2 = (* TODO *)
let s3 = (* TODO *)

let _ = CTR4.(show_sats print_var (all_sat s0))

(* Raymond §8.1: General scheme *)

module FORWARD1 (B : ENRICHED_BDD) = struct
  (** Q5. Implement the Forward Symbolic Model Checking scheme
          (Raymond, Figure 15). When [show] is true, you should
          show the satisfying assignements for the current reachability
          set using [show_sats print_var (all_sat s)]. *)
  let is_reachable
    ?(show=false)
    (s_init : B.t)
    (post_h : B.t -> B.t)
    (state_subst : (variable * B.t) list)
    (err : B.t) =
    failwith "not implemented" (* TODO *)
end

(* Q5. Confirm that state c3 is reachable using your answers to Q3 and Q5. *)
(* TODO *)

(** Q6. Adapt your solution to Q5 to include the optimization presented
        in the course (Raymond, Figure 15). *)
module FORWARD2 (B : ENRICHED_BDD) = struct
  let is_reachable
    ?(show=false)
    (s_init : B.t)
    (post_h : B.t -> B.t)
    (state_subst : (variable * B.t) list)
    (err : B.t) =
    failwith "not implemented" (* TODO *)
end

(* Q7. Apply the scheme above to test reachability of the simple counter. *)
(* TODO *)

(* Q8 (bonus). Write a function to generate a counter with n states and
               check reachability of the (n-1)th state.

     ↓
     0 -> 1 -> 2 -> ... -> n - 1
     ↑                      |
     +----------------------+
 *)

module type SYSTEM = sig
  include ENRICHED_BDD
  val init : t
  val trans : t
  val post_h : t -> t
  val state_subst : (variable * t) list
  val err : t
end

let simple_counter n =
  let print_var ff x =
    Format.fprintf ff "c%d%s" ((x - 1) mod n) (if (x - 1) < n then "" else "'")
  in
  let module CTR = (Enrich (val (Bdd.make ~print_var (2*n)))) in
  let c i = CTR.mk_var (i + 1) in
  let c' i = CTR.mk_var (n + i + 1) in
  (module struct
    include CTR
    let init = true        (* TODO *)
    let trans = true       (* TODO *)
    let post_h s = true    (* TODO *)
    let state_subst = [] (* TODO *)
    let err = c (n - 1)
   end : SYSTEM)

module CTR10 = (val (simple_counter 10))
module FORWARD1_CTR10 = FORWARD1 (CTR10)

let reachable =
  let open CTR10 in
  FORWARD1_CTR10.is_reachable ~show:true init post_h state_subst err

(* Going further: implement the optimized image computation described
   in Raymond §8.4. *)

(* Transforming (Boolean) Lustre programs into transition systems. *)

(* Going further: encode the following program using BDDs.

  node switch (orig, on, off : bool) returns (s : bool);
  let
    s = orig -> pre (if s then not off else on);
  tel
*)


