type variable = int
  
type aexp =
  | Aco of int              (* Aco : nat -> aexp *)
  | Ava of variable         (* Ava : nat -> aexp *)
  | Apl of aexp * aexp      (* Apl : aexp -> aexp -> aexp *)
  | Amu of aexp * aexp      (* Amu : aexp -> aexp -> aexp *)
  | Amo of aexp * aexp      (* Amo : aexp -> aexp -> aexp *)


type bexp =
  | Btrue
  | Bfalse
  | Bnot of bexp
  | Band of bexp * bexp
  | Bor  of bexp * bexp
  | Beq  of bexp * bexp     
  | Beqnat of aexp * aexp   

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


let rec evalB b s =
  match b with
  | Btrue -> true
  | Bfalse -> false
  | Bnot b' -> not (evalB b' s)
  | Band (b1, b2) -> (evalB b1 s) && (evalB b2 s)
  | Bor (b1, b2) -> (evalB b1 s) || (evalB b2 s)
  | Beq (b1, b2) -> (evalB b1 s) = (evalB b2 s)
  | Beqnat (n1, n2) -> (evalA n1 s) = (evalA n2 s)

type winstr =
  | Skip
  | Assign of variable * aexp
  | Seq    of winstr * winstr
  | If     of bexp * winstr * winstr
  | While  of bexp * winstr
type programme = winstr

type config =
  | Inter of winstr * ListState.t 
  | Final of ListState.t

let rec faire_un_pas (p : programme) (s : ListState.t) : config =
  match p with
  | Skip -> Final s
  | Assign (x, a) -> 
      let val_a = evalA a s in
      Final (ListState.update x val_a s)
  | Seq (i1, i2) -> 
      (match faire_un_pas i1 s with
       | Final s' -> Inter (i2, s')          
       | Inter (i1', s') -> Inter (Seq(i1', i2), s') 
      )
  | If (b, i1, i2) ->
      if evalB b s then Inter (i1, s) else Inter (i2, s)
  | While (b, i) ->
    Inter (If (b, Seq (i, While (b, i)), Skip), s)


let execute (prog : programme) : ListState.t =
  let rec loop (current_prog : programme) (current_state : ListState.t) : ListState.t =
    let result = faire_un_pas current_prog current_state in
    match result with
    | Final final_state -> final_state
    | Inter (next_prog, next_state) -> loop next_prog next_state
  in
  loop prog ListState.init

(*
--- TESTING ASSIGN AND SEQ ---
Program:
   variables -> [x; y]
   1. x := 10
   2. y := 5
   3. x := x - y  (Should result in 5) 
*)
let prog_calc = Seq(
  Assign(0, Aco 10),              (* Var[0] := 10 *)
  Seq(
    Assign(1, Aco 5),             (* Var[1] := 5 *)
    Assign(0, Amo(Ava 0, Ava 1))  (* Var[0] := Var[0] - Var[1] *)
  )
)

let final_state = execute prog_calc (*[5;5]*)

let () = assert (ListState.get 0 final_state = 5)
let () = assert (ListState.get 1 final_state = 5)


(*
   TESTING IF AND ASSIGN
Program: 
   1. x := 10
   2. If (x == 10) then y := 1 else y := 0
   Expected: y = 1
*)
let prog_if = Seq(
  Assign(0, Aco 10),              (* Var[0] := 10 *)
  If(
    Beqnat(Ava 0, Aco 10),        (* Condition: Var[0] == 10 *)
    Assign(1, Aco 1),             (* THEN: Var[1] := 1 *)
    Assign(1, Aco 0)              (* ELSE: Var[1] := 0 *)
  )
)

let state_if = execute prog_if

let () = assert (ListState.get 0 state_if = 10) 
let () = assert (ListState.get 1 state_if = 1)  (*cond true*)


(*
   TESTING WHILE
Program: 
   1. i := 0
   2. While (not (i == 3)) {
        i := i + 1
      }
*)
let prog_loop = Seq(
  Assign(0, Aco 0),                   (* Var[0] := 0 *)
  While(
    Bnot(Beqnat(Ava 0, Aco 3)),       (* Condition: NOT (i == 3) *)
    Assign(0, Apl(Ava 0, Aco 1))      (* Body: i := i + 1 *)
  )
)

let state_loop = execute prog_loop
let () = assert (ListState.get 0 state_loop = 3)
