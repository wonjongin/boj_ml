let rec bigpi u = match u with [] -> 1 | x :: v -> x * bigpi v
let get_char_as_string str pos = String.make 1 (String.get str pos)

let rec count_int_in_string str target pos =
  if pos >= String.length str then 0
  else
    (if get_char_as_string str pos = string_of_int target then 1 else 0)
    + count_int_in_string str target (pos + 1)

let ciisw target str = count_int_in_string str target 0

let () =
  let arr = Array.init 3 (fun _ -> read_int ()) in
  let num = Array.init 10 (fun x -> x) in
  let mul = arr |> Array.to_list |> bigpi |> Printf.sprintf "%d" in

  num |> Array.to_list
  |> List.iter (fun x -> mul |> ciisw x |> Printf.printf "%d\n")
