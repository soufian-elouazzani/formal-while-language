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



let s0 = ListState.init

(* SKIP *)
(* Expected: Final s0 *)
let res_skip = faire_un_pas Skip s0
let () = assert (res_skip = Final s0)

(* ASSIGN *)
(* Expected: Final (state with x=10) *)
let prog_assign = Assign (0, Aco 10)
let res_assign = faire_un_pas prog_assign s0

let s_with_10 = ListState.update 0 10 s0
let () = assert (res_assign = Final s_with_10)

(* Program: x := 5; y := 8 *)
let instr1 = Assign (0, Aco 5)
let instr2 = Assign (1, Aco 8)
let prog_seq = Seq (instr1, instr2)

(* --- STEP 1 --- *)
let res_seq_1 = faire_un_pas prog_seq s0
let s_with_5 = ListState.update 0 5 s0 (* Expected state after step 1 *)
let () = assert (res_seq_1 = Inter (instr2, s_with_5))

(* --- STEP 2 --- *)
let res_seq_2 = 
  match res_seq_1 with
  | Inter (p, s) -> faire_un_pas p s
  | Final _ -> failwith "Test error: should be Inter"

(* Expected: (Final), with x=5 and y=8 *)
let s_with_5_and_8 = ListState.update 1 8 s_with_5
let () = assert (res_seq_2 = Final s_with_5_and_8)


(* Program: If (1=1) then (x:=99) else (x:=0) *)
let condition = Beqnat (Aco 1, Aco 1) (* true *)
let branch_then = Assign (0, Aco 99)
let branch_else = Assign (0, Aco 0)
let prog_if = If (condition, branch_then, branch_else)

let res_if = faire_un_pas prog_if s0

(* Expected: Inter (branch_then, s0) *)
let () = assert (res_if = Inter (branch_then, s0))


(* Program: While (x=1) { ... } *)
let cond_w = Beqnat (Ava 0, Aco 1) 
let body_w = Skip
let prog_while = While (cond_w, body_w)

(* --- WHILE STEP --- *)
let res_while = faire_un_pas prog_while s0
let expected = If (cond_w, Seq(body_w, prog_while), Skip)

let () = assert (res_while = Inter (expected, s0))
