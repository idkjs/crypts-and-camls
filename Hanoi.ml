let () =
  let instruct i src dsk =
    Printf.printf "Moving disk %i from %s  to %s\n%!" i src dsk;
    Unix.sleep 1
  in
  let rec hanoi i src dsk aux =
    if i == 1 then instruct i src dsk else iterate i src dsk aux
  and iterate i src dsk aux =
    hanoi (i - 1) src aux dsk;
    instruct i src dsk;
    hanoi (i - 1) aux dsk src
  in
  hanoi 3 "A" "B" "C"

(* let () =
       let instruct i src dst = Printf.printf "Move disk %i from %s to %s\n%!" i src dst;Unix.sleep 1 in
       let rec hanoi i src dst aux = if i==1 then instruct i src dst else iterate i src dst aux and
               iterate i src dst aux = hanoi (i-1) src aux dst; instruct i src dst; hanoi (i-1) aux dst src in

   hanoi 7 "A" "B" "C" *)
