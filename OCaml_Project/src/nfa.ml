open List
open Sets

(*********)
(* Types *)
(*********)

type ('q, 's) transition = 'q * 's option * 'q

type ('q, 's) nfa_t = {
  sigma: 's list;
  qs: 'q list;
  q0: 'q;
  fs: 'q list;
  delta: ('q, 's) transition list;
}

(***********)
(* Utility *)
(***********)

(* explode converts a string to a character list *)
let explode (s: string) : char list =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l)
  in
  exp (String.length s - 1) []

(****************)
(* Part 1: NFAs *)
(****************)

let rec single_move (transitions: ('q * 's option * 'q) list) (state: 'q) (s: 's option) : 'q list =
  match transitions with
  [] -> []
  | h::t -> match h with
    (initial, ch, final) ->
      if initial = state then
        if ch = s then
          [final] @ single_move t state s
        else
          single_move t state s
      else
        single_move t state s

let rec move_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option): 'q list =
  match qs with
  [] -> []
  | h::t -> (single_move nfa.delta h s) @ (move_helper nfa t s)

let move (nfa: ('q,'s) nfa_t) (qs: 'q list) (s: 's option) : 'q list =
  insert_all (move_helper nfa qs s) []

let rec single_e_closure (nfa: ('q,'s) nfa_t) (transitions: ('q * 's option * 'q) list) (state: 'q) (visited: 'q list): 'q list =
  match transitions with
  [] -> []
  | h::t -> match h with
    (initial, ch, final) ->
      if initial = state && ch = None && not (mem final visited) then
        [final] @ single_e_closure nfa nfa.delta final (state::visited) @ single_e_closure nfa t state visited
      else
        single_e_closure nfa t state visited

let rec e_closure_helper (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  match qs with
  [] -> []
  | h::t -> single_e_closure nfa nfa.delta h [] @ [h] @ e_closure_helper nfa t 

let e_closure (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list =
  insert_all (e_closure_helper nfa qs) []

let rec check_finals (nfa: ('q, char) nfa_t) (qs: 'q list) : bool =
  match qs with
  [] -> false
  | h::t -> if exists (fun x -> x = h) nfa.fs then true else check_finals nfa t

let rec accept_helper (nfa: ('q, char) nfa_t) (str: char list) (states: 'q list) : bool =
  match str with
  | [] -> check_finals nfa (e_closure nfa states)
  | h::t ->
    match states with
      | [] -> false
      | _ -> accept_helper nfa t (e_closure nfa (move nfa states (Some h)));;

let accept (nfa: ('q, char) nfa_t) (s: string) : bool =
    accept_helper nfa (explode s) (e_closure nfa [nfa.q0]);;

(*******************************)
(* Part 2: Subset Construction *)
(*******************************)
let rec new_states_helper (nfa: ('q,'s) nfa_t) (sigma: 's list) (qs: 'q list) : 'q list list =
  match sigma with
  [] -> []
  | h::t -> [e_closure nfa (move nfa qs (Some h))] @ new_states_helper nfa t qs

let new_states (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  new_states_helper nfa nfa.sigma qs

let rec new_trans_helper (nfa: ('q,'s) nfa_t) (sigma: 's list) (qs: 'q list) : ('q list, 's) transition list =
  match sigma with
  [] -> []
  | h::t -> let second = e_closure nfa (move nfa qs (Some h)) in
  [(qs, (Some h), second)] @ new_trans_helper nfa t qs

let new_trans (nfa: ('q,'s) nfa_t) (qs: 'q list) : ('q list, 's) transition list =
  new_trans_helper nfa nfa.sigma qs

let rec new_finals_helper (fs: 'q list) (qs: 'q list) : bool =
  match qs with
  [] -> false
  | h::t -> if exists (fun x -> x = h) fs then true else new_finals_helper fs t

let new_finals (nfa: ('q,'s) nfa_t) (qs: 'q list) : 'q list list =
  if new_finals_helper nfa.fs qs then [qs] else []

let rec nfa_to_dfa_step (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t)
    (work: 'q list list) : ('q list, 's) nfa_t =
  match work with
  [] -> dfa
  | h::t ->
    let dfa2 = {
      qs= (new_states nfa h) @ dfa.qs;
      sigma= dfa.sigma;
      delta= (new_trans nfa h) @ dfa.delta;
      q0= dfa.q0;
      fs= []
    } in nfa_to_dfa_step nfa dfa2 t;;

let rec nfa_to_dfa_helper (nfa: ('q,'s) nfa_t) (dfa: ('q list, 's) nfa_t) (visited: 'q list list) : ('q list, 's) nfa_t =
  match visited with 
  [] -> let nfa = {
    sigma= dfa.sigma
    ; qs= dfa.qs
    ; delta=dfa.delta
    ; q0= dfa.q0
    ; fs= filter (fun x -> let new_finals = new_finals nfa x in if new_finals = [] then false else true) dfa.qs
  } in nfa
  | h::t -> let (new_states, new_transitions, end_visits) =
    fold_left (fun (curr_states, curr_trans, curr_visits) ch -> 
      let move_states = move nfa h (Some ch) in 
      let emove_states = e_closure nfa move_states in 
      if move_states = [] || exists (fun x -> x = emove_states) curr_states then
        (curr_states, curr_trans @ [(h, (Some ch), emove_states)], curr_visits) else
        (curr_states @ [emove_states], curr_trans @ [(h, (Some ch), emove_states)], curr_visits @ [emove_states]))
        (dfa.qs, dfa.delta, t) dfa.sigma
    in let dfa2 = {
      sigma= dfa.sigma
      ; qs= new_states
      ; q0= dfa.q0
      ; fs=dfa.fs
      ; delta=new_transitions
    } in nfa_to_dfa_helper nfa dfa2 end_visits;;

let nfa_to_dfa (nfa: ('q,'s) nfa_t) : ('q list, 's) nfa_t =
  let starts = e_closure nfa [nfa.q0] in
  let dfa =
    { qs= [starts]
    ; sigma= nfa.sigma
    ; delta= []
    ; q0= starts
    ; fs= []
  } in nfa_to_dfa_helper nfa dfa [starts];;