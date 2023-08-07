(* return the last elment of a list *)
let last lst = match List.rev lst with
  | [] -> None
  | h :: _  -> Some h

(* same problem but using recursion *)
let rec lastr = function
  | [] -> None
  | [a] -> Some a
  | _ :: t -> lastr t

let%test "test last function returns last element of list" =
  last [] = None &&
  last [1;2;3] = Some 3 &&
  last ["a"; "b"] = Some "b"

let%test "test last with recursion" =
  lastr [] = None &&
  lastr [1;2;3] = Some 3 &&
  lastr ["a"; "b"] = Some "b"
