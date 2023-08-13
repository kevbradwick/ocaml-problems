(* List problems              *)
(* https://ocaml.org/problems *)

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

let rev lst =
  let rec aux acc = function [] -> acc | a :: t -> aux (a :: acc) t in
  aux [] lst

let is_palindrome lst = rev lst = lst

type 'a node = One of 'a | Many of 'a node list

let flatten lst =
  let rec aux acc = function
    | [] -> acc
    | One a :: tail -> aux (a :: acc) tail
    | Many a :: tail -> aux (aux acc a) tail
  in
  rev (aux [] lst)

(* Logic - if two values are the same, drop them, otherwise recurse and push the first
   onto the next iteration *)
let rec compress = function
  | a :: (b :: _ as t) -> if a = b then compress t else a :: compress t
  | rest -> rest

let pack lst =
  (* current is a list of the same elements, accumulator is the new list that is returned *)
  let rec aux current acc = function
    | [] -> acc
    | [ x ] -> (x :: current) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (a :: current) acc t
        else aux [] ((a :: current) :: acc) t
  in
  rev (aux [] [] lst)

(*
   In -> encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
   Out <- (int * string) list = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")]
*)
let encode lst =
  let rec aux count acc = function
    | [] -> acc
    | [ x ] -> (count + 1, x) :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t else aux 0 ((count + 1, a) :: acc) t
  in
  rev (aux 0 [] lst)

type 'a rle = One of 'a | Many of int * 'a

let encode_modified lst =
  let create_t count ch = if count = 1 then One ch else Many (count, ch) in
  let rec aux count acc = function
    | [] -> acc
    | [ x ] -> create_t (count + 1) x :: acc
    | a :: (b :: _ as t) ->
        if a = b then aux (count + 1) acc t
        else aux 0 (create_t (count + 1) a :: acc) t
  in
  rev (aux 0 [] lst)

(*
#  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
- : string list = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
*)
and decode lst =
  let rec make_list char acc c =
    if c = 0 then acc else make_list char (char :: acc) (c - 1)
  in
  let rec aux acc = function
    | [] -> acc
    | One a :: tail -> aux (a :: acc) tail
    | Many (i, c) :: tail -> aux (make_list c acc i) tail
  in
  rev (aux [] lst)

let rec duplicate = function [] -> [] | a :: tail -> a :: a :: duplicate tail

let replicate lst b =
  let rec repeat ch acc = function
    | 0 -> acc
    | _ as i -> repeat ch (ch :: acc) (i - 1)
  in
  List.concat (List.map (fun ch -> repeat ch [] b) lst)

let%test "replicate test" = [ "a"; "a"; "b"; "b" ] = replicate [ "a"; "b" ] 2

let drop_nth lst n =
  let rec drop acc i = function
    | h :: t -> if i = n then drop acc 1 t else drop (h :: acc) (i + 1) t
    | [] -> acc
  in
  rev (drop [] 1 lst)

let%test "drop_nth" =
  drop_nth [ "a"; "b"; "c"; "d"; "e"; "f" ] 2 = [ "a"; "c"; "e" ]

let split lst n =
  let rec copy acc n = function
    | [] -> (acc, [])
    | h :: t ->
        if n = 0 then (List.rev acc, h :: t) else copy (h :: acc) (n - 1) t
  in
  copy [] n lst

let%test "split" =
  split [ "a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j" ] 3
  = ([ "a"; "b"; "c" ], [ "d"; "e"; "f"; "g"; "h"; "i"; "j" ])
