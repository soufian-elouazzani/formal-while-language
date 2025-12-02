(* ------------------------- *)
(* AST for expressions *)
(* ------------------------- *)
type variable = A | B | C | D

type expr =
  | Zero
  | One
  | Var of variable
  | Not of expr
  | And of expr * expr
  | Or of expr * expr

(* Helper: parse variable *)
let analyse_variable =
  terminal_res (function
    | 'a' -> Some A
    | 'b' -> Some B
    | 'c' -> Some C
    | 'd' -> Some D
    | _ -> None)

(* Helper: parse constants *)
let analyse_const =
  (terminal '0' -+> epsilon_res Zero)
  +| (terminal '1' -+> epsilon_res One)

(* ------------------------- *)
(* Expression parser *)
(* Grammar (non-left-recursive):
   E  ::= T E'
   E' ::= '+' T E' | ε
   T  ::= F T'
   T' ::= '.' F T' | ε
   F  ::= '!' F | '(' E ')' | A | C
*)

(* Forward declarations for recursion *)
let rec analyse_E lst = analyse_T ++> fun t ->
                        analyse_E_prime t lst

and analyse_E_prime t lst =
  ((terminal '+' -+> analyse_T ++> fun t2 ->
     analyse_E_prime (Or(t, t2))
  ) +| epsilon_res t) lst

and analyse_T lst = analyse_F ++> fun f ->
                    analyse_T_prime f lst

and analyse_T_prime f lst =
  ((terminal '.' -+> analyse_F ++> fun f2 ->
     analyse_T_prime (And(f, f2))
  ) +| epsilon_res f) lst

and analyse_F lst =
  (terminal '!' -+> analyse_F ++> fun f -> epsilon_res (Not f)
  +|
   (terminal '(' -+> analyse_E ++> fun e ->
      terminal ')' -+> epsilon_res e
   )
  +|
   analyse_const
  +|
   analyse_variable ++> fun v -> epsilon_res (Var v)
  ) lst
