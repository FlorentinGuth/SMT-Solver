type t = {
  parent  : int array;
  hi_node : int array;  (* Parent in the proof array *)
  rank    : int array;
}


let create n =
  {
    parent  = Array.init n (fun i -> i);
    hi_node = Array.init n (fun i -> i);
    rank    = Array.init n (fun i -> 0);
  }

let reset t =
  for i = 0 to Array.length t.parent - 1 do
    t.parent .(i) <- i;
    t.hi_node.(i) <- i;
    t.rank   .(i) <- 0;
  done


let rec find t a =
  let p = t.parent.(a) in
  if p = a then p else
    let pp = find t p in
    t.parent.(a) <- pp;
    pp

let highest_node t a =
  t.hi_node.(find t a)


let union t a b =
  (* t.t.proof.(a) = b *)
  let real_union i j pi pj =
    t.hi_node.(pi) <- highest_node t b;
    t.parent.(pj) <- pi
  in
  let pa = find t a in
  let pb = find t b in
  if pa <> pb then
    let ra = t.rank.(pa) in
    let rb = t.rank.(pb) in
    if ra > rb then
      real_union a b pa pb
    else if ra < rb then
      real_union b a pb pa
    else begin
      real_union a b pa pb;
      t.rank.(pa) <- ra + 1
    end


let test () =
  (* Proof tree: 0 <- 1 <- 2
                        <- 3
  *)
  let t = create 4 in
  union t 2 1;
  union t 3 1;
  Printf.printf "%d %d\n" (highest_node t 2) (highest_node t 3);
  union t 1 0;
  Printf.printf "%d %d\n" (highest_node t 2) (highest_node t 3)
(* let () = test () *)
