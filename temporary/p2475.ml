let sq x = x * x

let () =
  let nums =
    read_line () |> String.split_on_char ' ' |> List.map int_of_string
  in

  nums |> List.map sq |> List.fold_left ( + ) 0 |> fun x ->
  x mod 10 |> Printf.printf "%d"
