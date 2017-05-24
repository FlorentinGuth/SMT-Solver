module PArray = Persistent_array


type t = {
  mutable parent : int PArray.t;
  rank           : int PArray.t
}


let create n =
  {
    parent = PArray.init n (fun i -> i);
    rank   = PArray.init n (fun _ -> 0);
  }


(** @name  find_aux  - Auxiliary find function to search for a parent
    @param p         - The parent array
    @param i         - The element whose parent to search for
    @return (p', pi) - p' is an updated parent array, and pi the parent of i
*)
let rec find_aux p i =
  let pi = PArray.get p i in
  if pi == i then
    (p, i)
  else
    let (p', ppi) = find_aux p pi in
    (PArray.set p' i ppi, ppi)

let find t i =
  let (p, pi) = find_aux t.parent i in
  t.parent <- p;
  pi


let union t i j =
  let pi = find t i in
  let pj = find t j in
  if pi <> pj then
    let ri = PArray.get t.rank pi in
    let rj = PArray.get t.rank pj in
    if ri > rj then
      { t with parent = PArray.set t.parent pj pi }
    else if ri < rj then
      { t with parent = PArray.set t.parent pi pj }
    else
      { parent = PArray.set t.parent pj pi; rank = PArray.set t.rank pi (ri + 1) }
  else
    t

