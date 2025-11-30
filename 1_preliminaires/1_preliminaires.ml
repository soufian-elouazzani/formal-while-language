(* Exercice 1.1.1 *)

type variable = A | B | C | D

type expr =
  | Zero
  | One
  | Var of variable

type instruction =
  | Affectation of variable * expr
  | Seq of instruction * instruction
  | If of expr * instruction * instruction
  | While of expr * instruction
  | Skip

(* Exercice 1.1.2 *)
(**    
Prog        ::= Instr | Instr ';' Prog

Instr       ::= Affect
                | If
                | While

Affect      ::= Var ":=" Const

If          ::= 'i' '(' Var ')' '{' Prog '}' '{' Prog '}'

While       ::= 'w' '(' Var ')' '{' Prog '}'

Const       ::= '0' | '1'

Var         ::= 'a' | 'b' | 'c' | 'd'
    
*)


(* Exercice 1.1.3 Grammaire sans récursion gauche *) 
(* La récursion gauche est dans : 

Prog ::= Instr | Instr ';' Prog


On la transforme en :

Prog ::= Instr Prog'

Prog' ::= ';' Instr Prog' | ε

*)


(* Exercice 1.1.4 — Grammaire non récursive à gauche pour WHILEb *)

(* 
E  ::= T E'
E' ::= '+' T E' | ε

T  ::= F T'
T' ::= '.' F T' | ε

F  ::= '!' F
     | A
     | '(' E ')'

A ::= '0' | '1' | 'a' | 'b' | 'c' | 'd'

*)
