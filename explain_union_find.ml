(* Old implementation (2.1 of the paper):
   Union and find have the same complexity of the Tarjan implementation,
   and explain for a proof of size k has complexity O(k*log(n))
*)
(*
type t = {
  parent : int array;
  parent_no_path_compression : int array;
  mutable unions : (int * int * int * int) list;  (* (i, j, pi, pj) where pj points to pi *)
  rank   : int array;
}


let create n =
  {
    parent = Array.init n (fun i -> i);
    parent_no_path_compression = Array.init n (fun i -> i);
    unions = [];
    rank   = Array.init n (fun _ -> 0);
  }

(* Function find with provided parent array, and optional path compression *)
let rec find_aux parent path_cpr i =
    let pi = parent.(i) in
    if pi = i then i else
      let ppi = find_aux parent path_cpr pi in
      if path_cpr then parent.(i) <- ppi;
      ppi

let find t i =
  find_aux t.parent true i


let union t i j =
  let pi = find t i in
  let pj = find t j in
  if pi <> pj then
    let ri = t.rank.(pi) in
    let rj = t.rank.(pj) in
    if ri > rj then begin
      t.parent.(pj) <- pi;
      t.parent_no_path_compression.(pj) <- pi;
      t.unions <- (i, j, pi, pj) :: t.unions
    end else if ri < rj then begin
      t.parent.(pi) <- pj;
      t.parent_no_path_compression.(pi) <- pj;
      t.unions <- (j, i, pj, pi) :: t.unions
    end else begin
      t.parent.(pj) <- pi;
      t.parent_no_path_compression.(pj) <- pi;
      t.unions <- (i, j, pi, pj) :: t.unions;
      t.rank.(pi) <- ri + 1
    end


let find_no_pcr t i =
  find_aux t.parent_no_path_compression false i

let rec explain t i j =
  if i = j then [] else
    let (a, b) = explain_first t t.unions i j in
    List.rev_append (explain t b j) ((a, b) :: (explain t i a))
and explain_first t unions i j =
    match unions with
      | [] -> (* i and j were not equivalent *)
        (* Printf.printf "crashed on %d %d\n" i j; *)
        raise Not_found
      | (f, s, pf, ps) :: tl ->
        assert (t.parent_no_path_compression.(ps) = pf);
        t.parent_no_path_compression.(ps) <- ps;
        let res =
          let pi = find_no_pcr t i in
          let pj = find_no_pcr t j in
          if pi = pj then
            explain_first t tl i j
          else if pi = ps then
            (s, f)
          else
            let () = assert (pj = ps) in
            (f, s)
        in
        t.parent_no_path_compression.(ps) <- pf;
        res
*)


(* New implementation (2.2 of the paper):
   explain is O(k), but union is O(log(n))
*)

type t = {
  parent : int array;
  proof  : int array;  (* The proof forest *)
  rank   : int array;
}


let create n =
  {
    parent = Array.init n (fun i -> i);
    proof  = Array.init n (fun i -> i);
    rank   = Array.init n (fun _ -> 0);
  }

(* Function find with provided parent array, and optional path compression *)
let rec find_aux parent path_cpr i =
    let pi = parent.(i) in
    if pi = i then i else
      let ppi = find_aux parent path_cpr pi in
      if path_cpr then parent.(i) <- ppi;
      ppi

let find t i =
  find_aux t.parent true i

let find_proof t i =
  find_aux t.proof false i


let union t i j =
  let real_union i j pi pj =
    (* Performs the union, where pj points to pi *)
    t.parent.(pj) <- pi;
    (* Reversing of all edges from j to pj *)
    let rec reverse k sk =
      (* sk is the son of k, which will become its parent *)
      let pk = t.proof.(k) in
      t.proof.(k) <- sk;
      if pk <> k then reverse pk k
    in
    reverse j j;
    (* Finally performs the merge, j is now the representative of its class *)
    t.proof.(j) <- i
  in
  let pi = find t i in
  let pj = find t j in
  if pi <> pj then
    let ri = t.rank.(pi) in
    let rj = t.rank.(pj) in
    if ri > rj then
      real_union i j pi pj
    else if ri < rj then
      real_union j i pj pi
    else begin
      real_union i j pi pj;
      t.rank.(pi) <- ri + 1
    end


(* TODO not optimal complexity: O(height of tree) instead of O(k) *)
let explain t i j =
  let rec path k acc =
    let pk = t.proof.(k) in
    if k = pk then k :: acc else path pk (k :: acc)
  in
  let rec no_common_part u v removed =
    match (u, v) with
    | (x::xs, y::ys) when x = y -> no_common_part xs ys (x :: removed)
    | _ -> (u, v, removed)
  in
  let rec zip_2_by_2 l acc =
    match l with
    | [] -> assert false
    | [_] -> acc
    | x::(y::xs as tl) -> zip_2_by_2 tl ((x, y) :: acc)
  in
  let (pi, pj) = match no_common_part (path i []) (path j []) [] with
    | (pi, pj, []) -> (pi, pj)
    | (pi, pj, x :: _) -> (x :: pi, x :: pj)
  in
  zip_2_by_2 pi (zip_2_by_2 pj [])
