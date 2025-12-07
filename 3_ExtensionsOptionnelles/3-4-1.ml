type variable = int
  
type aexp =
  | Aco of int              (* Aco : nat -> aexp *)
  | Ava of variable         (* Ava : nat -> aexp *)
  | Apl of aexp * aexp      (* Apl : aexp -> aexp -> aexp *)
  | Amu of aexp * aexp      (* Amu : aexp -> aexp -> aexp *)
  | Amo of aexp * aexp      (* Amo : aexp -> aexp -> aexp *)

(*
— d’initialiser cet état (avec toutes les variables à 0) ;
   -> etat initial? Liste vide?
   
— de lire la valeur d’une variable ;
   ->get method in a list
   
— de modifier la valeur d’une variable ;
   ->update method in a list
   
— d’exécuter une instruction d’affectation.
   ->translate assign to ocaml? just use update?
   uses var and aexp -> translate aexp with evalA?
   update index_var value state
   translate expr to value --> evalA
   value = evalA expr state   -> update index_var value state ?
*)

module type STATE_SIG = sig
  type t
  val init : t
  val get : variable -> t -> int
  val update : variable -> int -> t -> t
end

module ListState : STATE_SIG = struct
  type t = int list
      
  let init = [] (*initial state*)

  (*get var*)
  let rec get id s = 
    match id, s with
    | 0, v :: _ -> v
    | n, _ :: q -> get (n - 1) q
    | _, [] -> 0

  (*update var*)
  let rec update i v = fun l ->
    match i, l with
    | 0, _ :: q -> v :: q                  
    | 0, []     -> [v]                     
    | n, x :: q -> x :: (update (n-1) v q) 
    | n, []     -> 0 :: (update (n-1) v []) 
end

let rec evalA expr st =
  match expr with
  | Aco n -> n
  | Ava v -> ListState.get v st
  | Apl (a1, a2) -> (evalA a1 st) + (evalA a2 st)
  | Amu (a1, a2) -> (evalA a1 st) * (evalA a2 st)
  | Amo (a1, a2) -> (evalA a1 st) - (evalA a2 st)

let exec_assign var_id expr st =
  let n = evalA expr st in     
  ListState.update var_id n st 

(*--- TESTS ---*)
    
(*testing "get" in "liste vide" -> needs to return 0*)
let () = assert (ListState.get 0 ListState.init = 0)
let () = assert (ListState.get 50 ListState.init = 0)

(*testing initialization, variable update and "imutabilité" *)
let s0 = ListState.init
let s1 = ListState.update 0 10 s0

let () = assert (ListState.get 0 s1 = 10) (*s1 changed*)
let () = assert (ListState.get 0 s0 = 0)  (*s0 didn't change*)


(*testing update adding new elements in the state*)
let s2 = ListState.update 2 99 s0
let () = assert (ListState.get 2 s2 = 99) (*[99]*)
let () = assert (ListState.get 1 s2 = 0)  (*[0; 99]*)
let () = assert (ListState.get 0 s2 = 0)  (*[0; 0; 99]*)
let s2' = ListState.update 1 12 s2
let s2' = ListState.update 0 7 s2'
let () = assert (ListState.get 1 s2' = 12)  (*[0; 12; 99]*)
let () = assert (ListState.get 0 s2' = 7)  (*[7; 12; 99]*)

(*testing evalA*)
(* 5 + (2 * 3) *)
let expr = Apl (Aco 5, Amu (Aco 2, Aco 3)) 
let () = assert (evalA expr s0 = 11)

(* 10 - 4 = 6 *)
let e1 = Amo (Aco 10, Aco 4) 
let () = assert (evalA e1 s0 = 6)

(* 6 * 7 = 42 *)
let e2 = Amu (Aco 6, Aco 7)
let () = assert (evalA e2 s0 = 42)

(* (2 + 3) * 4 = 20 *)
let e5 = Amu (Apl (Aco 2, Aco 3), Aco 4)
let () = assert (evalA e5 s0 = 20)



(*testing exec_assign*)
(* Var[1] := 50 in s0*)
let s3 = exec_assign 1 (Aco 50) s0
let () = assert (ListState.get 1 s3 = 50)
let () = assert (ListState.get 0 s3 = 0)
  
(*Var[4] := 42 in s3*)
let s3' = exec_assign 4 (Aco 42) s3
let () = assert (ListState.get 4 s3' = 42)
let () = assert (ListState.get 1 s3' = 50)
let () = assert (ListState.get 0 s3' = 0)
let () = assert (ListState.get 2 s3' = 0)
