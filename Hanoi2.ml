let () =
  let instruct i src dsk =
       Printf.printf "Moving disk %i from pole %s to pole %s\n%!" i src dsk;
       Unix.sleep 1
     in
  let rec hanoi n a b c =
    if n > 0 then (
      hanoi (n - 1) a c b;
      (* Printf.printf {j|Move disk from pole $a to pole $b|j}; *)
      (* Unix.sleep 1; *)
      instruct n c b;
      hanoi (n - 1) c b a);
  in

  hanoi 4 "1" "2" "3"
