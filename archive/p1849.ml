(* let insert_at index element lst = *)
(*   let rec aux i acc = function *)
(*     | [] -> if i = 0 then List.rev (element :: acc) else List.rev acc *)
(*     | h :: t -> *)
(*         if i = 0 then List.rev_append acc (element :: h :: t) *)
(*         else aux (i - 1) (h :: acc) t *)
(*   in *)
(*   aux index [] lst *)

(* let rec print_list = function *)
(*   | [] -> print_string "NO" *)
(*   | e :: l -> *)
(*       print_int e; *)
(*       print_string " "; *)
(*       print_list l *)

let () =
  let n = read_int () in
  let a = List.init n (fun _ -> read_int ()) in
  let res = Array.make n 0 in
  let pos = ref n in

  a |> List.rev
  |> List.iter (fun x ->
         decr pos;
         for i = n - 1 downto x + 1 do
           res.(i) <- res.(i - 1)
         done;
         res.(x) <- !pos + 1);
  res |> Array.iter (Printf.printf "%d\n")
