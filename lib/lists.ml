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
