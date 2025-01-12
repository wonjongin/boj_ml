let fac v =
  let rec helper v = match v with 0 -> 1 | 1 -> 1 | _ -> v * helper (v - 1) in
  helper v

let facs v i =
  let rec helper v i =
    match i with 0 -> 1 | _ -> v * helper (v - 1) (i - 1)
  in
  helper v i

let combi n r = facs n r / fac r

let h n r =
  let rr = r - n in
  combi (n + rr - 1) rr

let hs r =
  let max = float_of_int r /. float_of_int 3 |> Float.ceil |> int_of_float in
  let rec helper n r idx =
    if idx < max then 0
    else
      let dup =
        if r >= 9 && n == max then fac max / (fac 2 * fac (max - 2)) else 0
      in
      let this =
        if r - n - 3 >= 0 then h n r - (n * h n (r - 3)) + dup else h n r
      in
      (* Printf.printf "%d %d %d %d\n" n r (h n r) this; *)
      this + helper (n - 1) r (idx - 1)
  in
  helper r r r

let () =
  let n = read_int () in
  let nums = List.init n (fun _ -> read_int ()) in

  (* 중복조합으로 풀 수 있을 것 같은데 
     x + y + z = 10 이면 3 pi 10 이니까 
     저 미지수 개수를 2부터 주어진 수까지 해서 구하면 될 듯
     3이하가 되게하는 조건 추가해서...
     n pi r = n+r-1 C r 이었나... 
     근데 9 이상이고 n 작은 경우 441, 4411 (10에서) 같은거 나오는데 
     3이하 조건에서 중복돼서 빼줘야함
     *)
  nums |> List.map hs |> List.iter (Printf.printf "%d\n")

(* assert true; *)
(* assert (fac 6 == 720); *)
(* assert (combi 10 3 == 120); *)
(* assert (h 3 10 == combi 9 7) *)
