(* Generational reference. *)
type 'a gen = { mutable v : 'a ; mutable g : int }

type 'a rref = 'a gen

type data =
  | Mem
  | Invalid
  | Transaction of { mutable t : data ref ; mutable finished : bool }
  | Diff :
    { r : 'a rref ; mutable v : 'a ; mutable g : int ; mutable t : data ref }
    -> data

module type S = sig
  module Ref : sig
    type 'a t

    val ( := ) : 'a t -> 'a -> unit

    val ( ! ) : 'a t -> 'a
  end
end

let rec reroot (t : data ref) : unit =
  match !t with
  | Mem -> ()
  | Invalid -> assert false
  | Transaction ({ t = t' } as d) as n ->
    reroot t';
    t := Mem;
    d.t <- t;
    t' := n
  | Diff ({ r; v; g; t = t' } as d) as n ->
    reroot t';
    d.v <- r.v;
    d.g <- r.g;
    r.v <- v;
    r.g <- g;
    d.t <- t;
    t := Mem;
    t' := n

let rec invalidate (t : data ref) : unit =
  match !t with
  | Mem -> ()
  | Invalid -> assert false
  | Transaction { t = t' ; finished = false } ->
      assert false
  | Transaction { t = t' ; finished = true } ->
    invalidate t';
    t := Mem;
    t' := Invalid
  | Diff { r; v; g; t = t' } ->
    invalidate t';
    r.v <- v;
    r.g <- g;
    t := Mem;
    t' := Invalid

type snapshot = data ref gen

type store = snapshot

let create () : store = { v = ref Mem ; g = 0 }

let make (type a) (s : store) (v : a) : a rref = { s with v }

let capture (s : store) : snapshot =
  s.g <- s.g + 1;
  { v = s.v ; g = s.g }

let restore (s : store) (snap : snapshot) : unit =
  reroot snap.v;
  s.v <- snap.v;
  s.g <- snap.g

let get (type a) (_ : store) (r : a rref) : a = r.v

let set (type a) (s : store) (r : a rref) (v : a) : unit =
  if r.g = s.g then
    r.v <- v
  else
    let v' = r.v in
    let g' = r.g in
    r.v <- v;
    r.g <- s.g;
    let t' = ref Mem in
    s.v := Diff { r; v = v'; g = g'; t = t' };
    s.v <- t'

type transaction = data ref gen

let transaction (s : store) : transaction =
  let t' = ref Mem in
  s.v := Transaction { t = t' ; finished = false };
  let t = { v = s.v; g = s.g } in
  s.v <- t';
  s.g <- s.g + 1;
  t

let rollback (s : store) (t : transaction) : unit =
  match !(t.v) with
  | Transaction ({ t = t' ; finished = false } as d) ->
    invalidate t';
    s.v <- t';
    s.g <- t.g;
    d.finished <- true
  | _ -> assert false

let () =
  let s = create () in
  let r = make s 0 in
  let c = capture s in
  let t = transaction s in
  let t' = transaction s in
  set s r 2;
  let c' = capture s in
  Format.printf "A: %d@." (get s r);
  set s r 3;
  restore s c;
  (* Format.printf "B: %d@." (get s r);
  set s r 4;
  restore s c';  *)
  Format.printf "C: %d@." (get s r);
  set s r 5;
  rollback s t';
  Format.printf "D: %d@." (get s r);
  set s r 6;
  rollback s t;
  Format.printf "E: %d@." (get s r);
