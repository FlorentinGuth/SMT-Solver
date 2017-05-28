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
        Printf.printf "crashed on %d %d\n" i j ; raise Not_found
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
          else (* pj = ps *)
            (f, s)
        in
        t.parent_no_path_compression.(ps) <- pf;
        res
