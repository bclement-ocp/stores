type 'a rref = { mutable value : 'a ; mutable generation : int }

let rref value = { value; generation = -1 }

type data =
  Mem
| Diff : 'a rref * 'a * int * tree -> data
| Transaction of tree
| Invalid
and tree = data ref

let rec reroot t = match !t with
  | Mem -> t := Invalid
  | Diff (r, v, g, t') ->
      reroot t';
      t' := Invalid;
      r.value <- v;
      r.generation <- g;
  | Transaction t' ->
      reroot t';
      t' := Invalid
  | Invalid -> assert false

let reroot t = reroot t; t := Mem

type store = tree rref

let create () : store = { value = ref Mem ; generation = 0 }

let get (_ : store) (r : 'a rref) : 'a = r.value

let set (t : store) (r : 'a rref) (v : 'a) : unit =
  if t.generation = r.generation then
    r.value <- v
  else if r.value != v then (
    let t' = ref Mem in
    t.value := Diff (r, r.value, r.generation, t');
    t.value <- t';
    r.value <- v;
    r.generation <- t.generation
  )

type transaction = (tree * int) option ref

let transaction (t : store) : transaction =
  let trans = ref (Some (t.value, t.generation)) in
  let t' = ref Mem in
  t.value := Transaction t';
  t.value <- t';
  t.generation <- t.generation + 1;
  trans

let rollback (s : store) (trans : transaction) : unit =
  match !trans with
  | Some (t, g) ->
    trans := None;
    reroot t;
    s.value <- t;
    s.generation <- g
  | None -> assert false

let rec commit o g t = match !t with
  | Mem -> t := Invalid; o
  | Transaction t' -> t := Invalid; commit o g t'
  | Diff (r, v, g', t') ->
    t := Invalid;
    if g' > g then (
      commit o g t'
    ) else if g' = g then (
      r.generation <- g;
      commit o g t'
    ) else (
      r.generation <- g;
      let mem = ref Mem in
      o := Diff (r, v, g, mem);
      commit mem g t'
    )
  | Invalid -> assert false

let commit (s : store) (trans : transaction) : unit =
  match !trans with
  | Some (t, g) ->
    trans := None;
    s.value <- commit t g t;
    if s.value == t then s.value := Mem;
    s.generation <- g
  | None -> assert false

(* UnionFind interface *)
let new_store = create

let copy (_ : store) : store = assert false

let make (t : store) (v : 'a) : 'a rref =
  { value = v; generation = t.generation }

let eq (_ : store) (x : 'a rref) (y : 'a rref) : bool = (x == y)

let tentatively (s : store) (f : unit -> 'b) : 'b =
  let t = transaction s in
  try
    let result = f () in
    commit s t;
    result
  with e ->
    let b = Printexc.get_raw_backtrace () in
    rollback s t;
    Printexc.raise_with_backtrace e b
