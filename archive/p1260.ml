let dfs n v arr =
  let visited = Array.make (n + 1) false in

  let rec helper v =
    if not visited.(v) then (
      v |> Printf.printf "%d ";
      visited.(v) <- true;
      arr.(v) |> List.iter (fun x -> helper x))
  in
  helper v

let bfs n v arr =
  let visited = Array.make (n + 1) false in
  let next = Queue.create () in
  next |> Queue.push v;
  let rec helper idx =
    if next |> Queue.is_empty then ()
    else
      let cur = next |> Queue.pop in
      if not visited.(cur) then (
        visited.(cur) <- true;
        cur |> Printf.printf "%d ";
        arr.(cur)
        |> List.iter (fun x -> if not visited.(x) then next |> Queue.push x));
      helper (idx + 1)
  in
  helper 1

let () =
  let n, m, v =
    match
      read_line () |> String.split_on_char ' ' |> List.map int_of_string
    with
    | [ a; b; c ] -> (a, b, c)
    | _ -> failwith "Not Three"
  in

  let arr = Array.make (n + 1) [] in

  for _ = 1 to m do
    let a, b =
      match
        read_line () |> String.split_on_char ' ' |> List.map int_of_string
      with
      | [ a; b ] -> (a, b)
      | _ -> failwith "Not Two"
    in
    arr.(a) <- b :: arr.(a);
    arr.(b) <- a :: arr.(b)
  done;

  arr |> Array.iteri (fun i x -> arr.(i) <- x |> List.sort compare);
  arr |> dfs n v;
  print_newline ();
  arr |> bfs n v
