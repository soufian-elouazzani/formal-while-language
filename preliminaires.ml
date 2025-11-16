(* Type pour les variables *)
type variable = A | B | C | D

(* Type pour les expressions (très simple dans WHILEb--) *)
type expression =
  | Zero                    (* 0 *)
  | One                    (* 1 *)
  | Var of variable         (* Une variable: a, b, c, ou d *)


(* Type pour les programmes *)
type programme =
  | Skip                    (* Ne rien faire *)
  | Assign of variable * expression  (* Affectation: var := expr *)
  | Seq of programme * programme     (* Séquence: prog1 ; prog2 *)
  | If of expression * programme * programme  (* Conditionnelle *)
  | While of expression * programme  (* Boucle while *)


(* Exercice 1.1.2 *)

(**
Program  ::= Instruction

Instruction ::=
  | ";"                    (* ne rien faire *)
  | Affectation            (* affectation *)
  | Instruction ";" Instruction  (* séquence *)
  | "i" "(" Variable ")" "{" Instruction "}" "{" Instruction "}"  (* if *)
  | "w" "(" Variable ")" "{" Instruction "}"                     (* while *)
  |

Affectation ::= Variable ":=" Expression

Expression ::=
  | "0"
  | "1" 
  | Variable

   Variable ::= "a" | "b" | "c" | "d" *)

(* Exercice 1.1.3 *)

