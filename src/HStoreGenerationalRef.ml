let gengen = let c = ref 0 in fun () -> incr c; !c

type 'a rref = { mutable value : 'a ; mutable generation : int }

let rref value = { value ; generation = gengen () }

type tree = Mem | Diff : 'a rref * 'a * tree ref -> tree

let rec reroot t = match !t with
| Mem -> ()
| Diff (r, v, t') ->
  reroot t';
  t := Mem;
  t' := Diff (r, r.value, t);
  r.value <- v

type snapshot = tree ref

type t = tree ref rref

let create () = { value = ref Mem ; generation = gengen () }

let capture t =
  t.generation <- gengen ();
  t.value

let restore t s =
  reroot s;
  t.value <- s;
  t.generation <- gengen ()

let get _ r = r.value

let set t r v =
  if t.generation = r.generation then
    r.value <- v
  else (
    let t' = ref Mem in
    t.value := Diff (r, r.value, t');
    t.value <- t';
    r.value <- v;
    r.generation <- t.generation
  )

(* UnionFind interface *)
type store = t

let new_store () = create ()

let copy _ = assert false

let make _ v = rref v

let eq _ (x : 'a rref) (y : 'a rref) = (x == y)

let tentatively (s : store) (f : unit -> 'b) : 'b =
  let snap = capture s in
  try f () with e ->
    let b = Printexc.get_raw_backtrace () in
    restore s snap;
    Printexc.raise_with_backtrace e b

