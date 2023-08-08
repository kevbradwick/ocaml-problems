let rec last = function [] -> None | [ a ] -> Some a | _ :: t -> last t

(*
   Last Two elements of a list

   Recursion works here because you can pattern match the list with exactly two
   elements.
*)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: t -> last_two t

(*
   N'th element of a list
*)
let rec nth index = function
  | [] -> None
  | h :: t -> if index = 1 then Some h else nth (index - 1) t

let length lst =
  let rec aux count acc =
    match acc with
    | [] -> count
    | _ :: t -> if t = [] then count + 1 else aux (count + 1) t
  in
  aux 0 lst
