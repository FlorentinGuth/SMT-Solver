type t = {
  parent : int array;
  rank   : int array;
}


let create n =
  {
    parent = Array.init n (fun i -> i);
    rank   = Array.init n (fun _ -> 0);
  }



let find t i =
  let rec find_aux i =
    let pi = t.parent.(i) in
    if pi == i then
      i
    else
      let ppi = find_aux pi in
      t.parent.(i) <- ppi;
      ppi
  in
  find_aux i


let union t i j =
  let pi = find t i in
  let pj = find t j in
  if pi <> pj then
    let ri = t.rank.(pi) in
    let rj = t.rank.(pj) in
    if ri > rj then
      t.parent.(pj) <- pi
    else if ri < rj then
      t.parent.(pi) <- pj
    else begin
      t.parent.(pj) <- pi;
      t.rank.(pi) <- ri + 1
    end
