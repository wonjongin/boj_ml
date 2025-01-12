module BinaryTree = struct
  type binarytree = { mutable data : int Array.t; mutable length : int }
  (* type 'a t = 'a binarytree *)

  let is_empty (b : binarytree) : bool =
    match b.length with 1 -> true | _ -> false

  let create n = { data = Array.make (n + 1) 0; length = 0 }
  let get_root b = match b.length with 0 -> 0 | _ -> b.data.(1)
  let get_myself i b = b.data.(i)
  let get_parent i b = b.data.(i / 2)
  let get_children i b = (b.data.(2 * i), b.data.((2 * i) + 1))
  (* let set_myself i b x = b.data.(i) <- x *)
  (* let set_parent i b x = b.data.(i / 2) <- x *)

  let change i b =
    let temp = b |> get_myself i in
    b.data.(i) <- b |> get_parent i;
    b.data.(i / 2) <- temp

  (* 부모와 자신을 비교 자신이 크면 true *)
  let cmp_p i b = b |> get_myself i > (b |> get_parent i)

  let push v b =
    let idx = b.length + 1 in
    b.data.(idx) <- v;
    b.length <- idx + 1;
    idx

  let push_heap v b =
    let idx = b |> push v in
    let rec helper idx =
      if b |> cmp_p idx = true then (
        b |> change idx;
        helper (idx / 2))
      else ()
    in
    helper idx
end

let () =
  let n = read_int () in
  let tree = BinaryTree.create n in
  for _ = 1 to n do
    let x = read_int () in
    match x with
    | 0 -> tree |> BinaryTree.get_root |> Printf.printf "%d\n"
    | _ -> tree |> BinaryTree.push_heap x
  done
