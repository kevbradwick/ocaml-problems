(* return the last elment of a list *)
let last lst = match List.rev lst with [] -> None | h :: _ -> Some h

(* same problem but using recursion *)
let rec lastr = function [] -> None | [ a ] -> Some a | _ :: t -> lastr t

let%test "test last function returns last element of list" =
  last [] = None && last [ 1; 2; 3 ] = Some 3 && last [ "a"; "b" ] = Some "b"

let%test "test last with recursion" =
  lastr [] = None && lastr [ 1; 2; 3 ] = Some 3 && lastr [ "a"; "b" ] = Some "b"

(*
   Last Two elements of a list

   Recursion works here because you can pattern match the list with exactly two
   elements.
*)
let rec last_two = function
  | [] | [ _ ] -> None
  | [ a; b ] -> Some (a, b)
  | _ :: t -> last_two t

let%test "last two with recursion" =
  last_two [] = None
  && last_two [ 1 ] = None
  && last_two [ 1; 2; 3; 4 ] = Some (3, 4)

(*
   N'th element of a list
*)
let nth lst index =
  let rec aux count = function
    | [] -> None
    | h :: t -> if count = index then Some h else aux (count + 1) t
  in
  aux 0 lst

let%test "get the nth element of a list" =
  nth [] 2 = None && nth [ 1; 2; 3 ] 1 = Some 2 && nth [ 1; 2; 3 ] 5 = None

let length lst =
  let rec aux count acc =
    match acc with
    | [] -> count
    | _ :: t -> if t = [] then count + 1 else aux (count + 1) t
  in
  aux 0 lst

let%test "test length of list" =
  length [] = 0 && length [ 1 ] = 1 && length [ 1; 2; 3; 4 ] = 4
