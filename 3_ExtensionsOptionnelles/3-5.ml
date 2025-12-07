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

(*just added count+1 in each Inter config*)
let execute_with_count (prog : programme) : ListState.t =
  let rec loop (current_prog : programme) (current_state : ListState.t) (count : int) : ListState.t =
    let result = faire_un_pas current_prog current_state in
    match result with
    | Final final_state -> 
        Printf.printf "Execution finished in %d steps.\n" count; 
        final_state
    | Inter (next_prog, next_state) -> 
        loop next_prog next_state (count + 1)
  in
  loop prog ListState.init 1


(* : 1 step (Assign -> Final) *)
let prog1 = Assign(0, Aco 42)
let _ = execute_with_count prog1

(*
1) Seq -> exec Assign,
2) exec the other Assign
= 2 steps
*)
let prog2 = Seq(Assign(0, Aco 1), Assign(1, Aco 2))
let _ = execute_with_count prog2

(*
Program:
   x := 3
   While (x != 0) {
        x := x - 1
      }

   1) Seq (1assign, 2While)                                            x = 3
   2) While ->  (3if i1 skip) where i1 = Seq (4assign, 5While)         x = 2
                 6                            7        8               x = 1
                 9                            10       11              x = 0
                 12      13

   13 steps
*)

let prog3 = Seq(
  Assign(0, Aco 3),               (* Var[0] := 3 *)
  While(
    Bnot(Beqnat(Ava 0, Aco 0)),   (* While x != 0 *)
    Assign(0, Amo(Ava 0, Aco 1))  (* x := x - 1 *)
  )
)

let final_state_simple = execute_with_count prog3
let () = assert (ListState.get 0 final_state_simple = 0) (*verify x=0*)
