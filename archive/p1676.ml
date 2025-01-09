
let rec power5 x = 
  if x = 1 then 5
  else 5 * power5 ( x-1 )

let () =
  let n = read_int () in 
  let res = ref 0 in
  for i = 1 to 3 do
    res := !res + n / power5 i
  done;
  Printf.printf "%d\n" !res
