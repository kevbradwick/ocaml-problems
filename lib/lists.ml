(* return the last elment of a list *)
let last lst = match List.rev lst with
  | [] -> None
  | h :: _  -> Some h

let%test "test last function returns last element of list" =
  last [] = None &&
  last [1;2;3] = Some 3 &&
  last ["a"; "b"] = Some "b"
