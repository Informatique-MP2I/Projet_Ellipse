(*
   in      : flottant a and b 
   precond : a>0 and b>0
   out     : perimeter of an ellipse via ramanujan
*)
let ramanujan a b =
  if (a<=0.) || (b<=0.) then
    failwith "ramanujan. Wrong parameters, need a>0 and b>0."
  else
    let h=((a-.b)*.(a-.b)) /. ((a+.b)*.(a+.b)) in
    Float.pi *.(a +. b)*.(1. +. 3.*.h /. (10. +. sqrt(4. -. 3. *. h)))

(*
   in  : two points given each as a pair of (float, float)
   out : the distance between these two points
*)
let distance point1 point2 =
  let x1 = fst point1 in
  let y1 = snd point1 in
  let x2 = fst point2 in
  let y2 = snd point2 in
  sqrt((x1 -. x2) *. (x1 -. x2) +. (y1 -. y2) *. (y1 -. y2))

(*
   in      : n integer
   precond : n>=2
   out     : the perimeter of the inscribed polygon
*)
let approx_perim_ellipse1 n a b =
  if n<2 then
    failwith "approx_perim_ellipse1. Wrong parameter, need n>=2."
  else
    let p = int_of_float(2.**float_of_int(n)) in
    let perim = ref 0. in
    for i=0 to p-1 do
      let a1 = float_of_int(i) *. 2. *. Float.pi /. float_of_int(p) in
      let a2 = float_of_int(i+1) *. 2. *. Float.pi /. float_of_int(p) in
      let point1 = (a *. (cos a1), b *. (sin a1)) in
      let point2 = (a *. (cos a2), b *. (sin a2)) in
      perim := !perim +. (distance point1 point2)
    done;
    !perim

(*
   in      : n integer
   precond : n>=2
   out     : the perimeter of the inscribed polygon
*)
let approx_perim_ellipse2 n a b =
  if n<2 then
    failwith "approx_perim_ellipse2. Wrong parameter, need n>=2."
  else
    let rec aux n a b theta1 theta2 =
      if n=0 then
        let point1 = (a *. (cos theta1), b *. (sin theta1)) in
        let point2 = (a *. (cos theta2), b *. (sin theta2)) in
        distance point1 point2
      else
        (aux (n-1) a b theta1 ((theta1+.theta2)/.2.)) +.
        (aux (n-1) a b ((theta1+.theta2)/.2.) theta2)
    in
    aux n a b 0. (2. *. Float.pi)
      


(* Main expression *)
let ()=
  (* Ex2 - Q1 et Q2 *)
  let a=6. and b=3. in
  Printf.printf "Perimeter of the ellipse for a=%.2f and b=%.2f using Ramanujan's formula is %.10f.\n" a b (ramanujan a b);
  (* Ex3 - Q1 *)
  let point1=(5.,0.) and point2=(2.,4.) in
  Printf.printf "Distance between (%f,%f) and (%f,%f) is %f.\n" (fst point1) (snd point1) (fst point2) (snd point2) (distance point1 point2);
  (* Ex3 - Q3 *)
  let n = 5 and a=6. and b=3. in
  Printf.printf "n=%d. Ellipse perimeter : %f.\n" n (approx_perim_ellipse1 n a b);
  (* Ex3 - Q5 *)
  let n = 5 and a=1. and b=1. in
  let approx_pi = ((approx_perim_ellipse1 n a b)/. 2.) in
  Printf.printf "n=%d. Pi : %.20f.\n" n approx_pi;
  (* Ex3 - Q5 *)
  let a=1. and b=1. in
  for n=5 to 29 do
    let approx_pi1 = ((approx_perim_ellipse1 n a b)/. 2.) in
    Printf.printf "n=%d. Difference with pi : %.20f\n" n (Float.pi -. approx_pi1);
    flush stdout
  done;
  (* Ex4 - Q1 *)
  let n = 5 and a=6. and b=3. in
  Printf.printf "n=%d. Ellipse perimeter : %f.\n" n (approx_perim_ellipse2 n a b);
  (* Ex4 - Q2 *)
  let n = 5 and a=1. and b=1. in
  let approx_pi = ((approx_perim_ellipse2 n a b)/. 2.) in
  Printf.printf "n=%d. Pi : %.20f.\n" n approx_pi;
  (* Ex4 - Q3 *)
  let a=1. and b=1. in
  for n=5 to 29 do
    let approx_pi2 = ((approx_perim_ellipse2 n a b)/. 2.) in
    Printf.printf "n=%d. Difference with pi : %.20f\n" n (Float.pi -. approx_pi2);
    flush stdout
  done;
  (* Comparaison of two methods *)
  let a=1. and b=1. in
  for n=5 to 29 do
    let approx_pi1 = ((approx_perim_ellipse1 n a b)/. 2.) in
    let approx_pi2 = ((approx_perim_ellipse2 n a b)/. 2.) in
    Printf.printf "n=%d. Difference (1) : %.20f\n" n (Float.pi -. approx_pi1);
    Printf.printf "n=%d. Difference (2) : %.20f\n" n (Float.pi -. approx_pi2);
    flush stdout
  done
