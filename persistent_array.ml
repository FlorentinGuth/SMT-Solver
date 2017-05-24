(** An array is either an immediate array, or a persistent array with a modification *)
type 'a t = 'a data ref
and 'a data =
  | Arr  of 'a array
  | Diff of int * 'a * 'a t


let init n f =
  ref (Arr (Array.init n f))


(*
(** Ensures t is an immediate array (in place), and returns it *)
let rec reroot t =
  match !t with
  | Arr a -> a
  | Diff (i, x, t') ->
    let a = reroot t' in
    let x' = a.(i) in
    a.(i) <- x;
    t  := Arr a;
    t' := Diff (i, x', t);
    a
*)

(** Same function, but tail-recursive with continuation-passing style.
    That is, with return k a instead of a, for the given function k *)
let rec reroot' t k =
  match ! t with
  | Arr a -> k a
  | Diff (i, x, t') ->
    reroot' t' (fun a ->
                 let x' = a.(i) in
                 a.(i) <- x;
                 t := Arr a;
                 t' := Diff (i, x', t);
                 a)
let reroot t =
  reroot' t (fun a -> a)

let rec get t i =
  match !t with
  | Arr a -> a.(i)
  | Diff _ -> let a = reroot t in a.(i)

let set t i x =
  let a = reroot t in
  let x' = a.(i) in
  let t' = ref (Arr a) in
  a.(i) <- x;
  t := Diff (i, x', t');
  t'
