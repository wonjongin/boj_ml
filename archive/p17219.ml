let () =
  let n, m =
    match
      read_line () |> String.split_on_char ' ' |> List.map int_of_string
    with
    | [ a; b ] -> (a, b)
    | _ -> failwith "Not two"
  in
  let table = Hashtbl.create n in
  for _ = 1 to n do
    let host, pw =
      match read_line () |> String.split_on_char ' ' with
      | [ a; b ] -> (a, b)
      | _ -> failwith "Not two"
    in

    Hashtbl.add table host pw
  done;
  let finds = List.init m (fun _ -> read_line ()) in

  finds
  |> List.iter (fun x ->
         Hashtbl.find table x |> print_string;
         print_newline ())
