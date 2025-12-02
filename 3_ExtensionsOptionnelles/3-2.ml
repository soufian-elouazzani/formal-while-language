#use "anacomb.ml"

(* ---------------------- TOKENS ---------------------- *)

type token =
  | Tident of string
  | Tnombre of int
  | Tplus
  | Tmoins
  | Tfois
  | Tdiv
  | Tegal
  | TparO
  | TparF
  | Tpointvirgule
  | Tsi
  | Talors
  | Tsinon
  | Tfait
  | Ttantque
  | Tfaire

(* ---------------------- AST ---------------------- *)

type expr =
  | EConst of int
  | EVar of string
  | EPlus of expr * expr
  | EMoins of expr * expr
  | ETimes of expr * expr
  | EDiv of expr * expr

type instr =
  | Affect of string * expr
  | Suite of instr list
  | Si of expr * instr * instr
  | Tantque of expr * instr

(* ---------------------------------------------------------- *)
(* TERMINAUX : conversion token -> AST                        *)
(* ---------------------------------------------------------- *)

let t_ident =
  terminal_res (function Tident s -> Some s | _ -> None)

let t_nombre =
  terminal_res (function Tnombre n -> Some (EConst n) | _ -> None)

let t_symbole tok =
  terminal tok

(* ---------------------------------------------------------- *)
(* ANALYSEUR D'EXPRESSIONS  (VERSION COMPACTE)                *)
(* ---------------------------------------------------------- *)

(* forward declarations *)
let rec a_expr l = expr_add l
and expr_add l =
  ((expr_mul ++> fun e1 ->
     ((t_symbole Tplus -+> expr_add) ++> fun e2 ->
        epsilon_res (EPlus(e1,e2)))
     +|
     ((t_symbole Tmoins -+> expr_add) ++> fun e2 ->
        epsilon_res (EMoins(e1,e2)))
   )
   +|
   expr_mul) l

and expr_mul l =
  ((expr_atom ++> fun e1 ->
     ((t_symbole Tfois -+> expr_mul) ++> fun e2 ->
        epsilon_res (ETimes(e1,e2)))
     +|
     ((t_symbole Tdiv -+> expr_mul) ++> fun e2 ->
        epsilon_res (EDiv(e1,e2)))
   )
   +|
   expr_atom) l

and expr_atom l =
  ( t_nombre
    +|
    ( t_ident ++> fun name -> epsilon_res (EVar name) )
    +|
    ( t_symbole TparO -+> a_expr ++> fun e ->
        t_symbole TparF -+> epsilon_res e )
  ) l

(* ---------------------------------------------------------- *)
(* INSTRUCTIONS                                                *)
(* ---------------------------------------------------------- *)

let rec a_instr l =
  (
    (* affectation : ident = expr ; *)
    (t_ident ++> fun name ->
       t_symbole Tegal -+> a_expr ++> fun e ->
         t_symbole Tpointvirgule -+> epsilon_res (Affect (name, e))
    )

    +|

    (* if expr then instr else instr *)
    (t_symbole Tsi -+> a_expr ++> fun e ->
       t_symbole Talors -+> a_instr ++> fun i1 ->
         t_symbole Tsinon -+> a_instr ++> fun i2 ->
           epsilon_res (Si(e,i1,i2))
    )

    +|

    (* while expr do instr *)
    (t_symbole Ttantque -+> a_expr ++> fun e ->
       t_symbole Tfaire -+> a_instr ++> fun i ->
         epsilon_res (Tantque(e,i))
    )

    +|

    (* bloc : instr instr ... fait *)
    (t_symbole Tfait -+> epsilon_res (Suite []))
  ) l

(* Séquence : suite d'instructions jusqu'à 'fait' *)
and a_suite l =
  (
    (a_instr ++> fun i ->
       a_suite ++> fun (Suite rest) ->
         epsilon_res (Suite (i :: rest))
    )
    +|
    (t_symbole Tfait -+> epsilon_res (Suite []))
  ) l

(* programme = suite d’instructions *)
let a_programme = a_suite

