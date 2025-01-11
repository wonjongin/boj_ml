let count value coins =
  let rec helper value coins acc =
    match coins with
    | [] -> 0
    | e :: u -> ((value - (value mod e)) / e) + helper (value mod e) u acc
  in
  helper value coins 0

let () =
  let n, k =
    match
      read_line () |> String.split_on_char ' ' |> List.map int_of_string
    with
    | [ a; b ] -> (a, b)
    | _ -> failwith "Not Two"
  in

  let coins = List.init n (fun _ -> read_int ()) in

  coins |> List.rev |> count k |> print_int
