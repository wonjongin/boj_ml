let pow base exp =
  let rec helper base exp acc =
    if exp = 0 then acc
    else
      let res = Int64.rem (Int64.mul acc base) 1234567891L in
      helper base (exp - 1) res
  in
  helper base exp 1L

(* let hash u = *)
(* let rec helper lst idx =  *)
(*   match lst with *)
(*   | [] -> 0 *)
(*   | x :: v -> ((x * (pow 31 idx )) mod 1234567891) + helper v (idx + 1) *)
(* in *)
(* helper u 0 *)

let str_to_list s = s |> String.to_seq |> List.of_seq

let () =
  let _ = read_int () in
  let str = read_line () in
  str |> str_to_list |> List.map int_of_char
  |> List.map (fun x -> x - 96)
  |> List.mapi (fun i x ->
         let lx = x |> Int64.of_int in
         let lp = pow 31L i in
         Int64.rem (Int64.mul lx lp) 1234567891L
         (* ((x * (pow 31 i )) mod 1234567891) *))
  |> List.fold_left (fun acc x -> Int64.rem (Int64.add acc x) 1234567891L) 0L
  |> Printf.printf "%Ld\n"
