let () =
  let n = read_int () in
  let nums =
    read_line () |> String.split_on_char ' ' |> List.map int_of_string
  in

  let table = Hashtbl.create n in

  nums |> List.sort_uniq compare
  |> List.iteri (fun i x -> Hashtbl.add table x i);

  nums |> List.iter (fun x -> Hashtbl.find table x |> Printf.printf "%d ")
