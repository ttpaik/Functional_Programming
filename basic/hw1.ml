(*
Honor code comes here:

First Name: Taehyon 
Last Name: Paik
BU ID: U58182574

I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.
*)

(* 
a print_list function useful for debugging.
*)

let rec print_list (ls: int list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | e::[] -> print_int e
    | e::l -> 
      let _ = print_int e 
      in let _ = print_string "; " 
      in aux l

  in let _ = print_string "[" 
  in let _ = aux ls
  in         print_string "]" 


(* Problems *)

(*
TODO: Write a function called between that lists the integers between two integers (inclusive)
If the first number is greater then the second return the empty list
the solution should be tail recursive

For example,
between 4 7 = [4; 5; 6; 7]
between 3 3 = [3]
between 10 2 = []
between 4 1000000 does not stack overflow
*)

let rec b2 n e accum =
  if n>e then accum 
  else if n==e then n::accum
  else b2 n (e-1) (e::accum)

let rec between (n:int) (e:int): int list = 
  b2 n e []


(*
TODO: Write a zip function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, combine as long as possible
your method should be tail recursive.

For example,
zip_int [1;2;3;5] [6;7;8;9] = [(1,6);(2,7);(3,8);(5,9)]
zip_int [1] [2;4;6;8] = [(1,2)]
zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
*)
let rec print_tl (ls: (int*int) list): unit =
  let rec aux ls = match ls with
    | [] -> print_string ""
    | (a, b) :: [] -> Printf.printf "(%i, %i)" a b
    | (a, b) :: rest ->
      let _= Printf.printf "(%i, %i)" a b
      in let _= print_string"; "
      in aux rest
  in print_string"[ "; aux ls; print_string " ]"


let zip_int (a: int list) (b: int list): (int * int) list = 
  let rec zip2 a b accum = 
    match a, b with 
    | [], [] -> List.rev accum
    | h1::[], h2::t2 -> List.rev ((h1, h2)::accum)
    | h1::t1, h2::[] -> List.rev ((h1, h2)::accum)
    | h1::t1, h2::t2 -> zip2 t1 t2 ((h1, h2)::accum)
    | [], h2::t2 -> List.rev accum
    | h1::t1, [] -> List.rev accum
  in zip2 a b []



(*
TODO: Write a dotProduct function for lists of integers,
If the two list are of unequal lengths then return 0

For example,
dotProduct [1;2;3;4] [6;7;8;9] = 80            (since 1*6+2*7+3*8+4*9 = 80)
dotProduct [1;2;3;4] [6] = 0
*)

let rec sum lis =
  let rec sub_sum accum = function
    | [] -> accum
    | h::t -> sub_sum (accum+h) t
  in
  sub_sum 0 lis

let rec dpo (c: int list) (d: int list): int list =
  let rec dp c d accum = 
    match c, d with 
    | [], [] -> accum
    | h1::[], h2::[] -> (h1*h2)::accum
    | h1::[], h2::t2 -> []
    | h1::t1, h2::[] -> []
    | h1::t1, h2::t2 -> dp t1 t2 ((h1*h2)::accum)
    | [], h2::t2 -> []
    | h1::t1, [] -> []
  in dp c d []

let rec dotProduct (x: int list) (y: int list): int = 
  let a = (dpo x y)
  in sum a


(* 
TODO:
Write a function that takes a list of tuples and returns a string representation of that list

your representation should be valid as OCaml source:
* every element of a list must be separated by ";"
* the list must be wrapped in "[" and "]"
* tuples should (1,2)
* You may use whitespace however you like

For example,
list_of_tuple_as_string [(1,2);(3,4);(5,6)] = "[ (1,2); (3,4); (5,6) ]"
*)

let rec tostr (newl: (int*int) list): string =
  match newl with
  | [] -> ""
  | ((a:int), (b:int)) :: [] -> 
    "(" ^ string_of_int a ^ "," ^ string_of_int b ^ ")"
  | ((a:int), (b:int)) :: rem -> 
    "(" ^ string_of_int a ^ "," ^ string_of_int b ^ "); " ^ tostr rem

let rec list_of_tuple_as_string (list: (int*int) list): string =
  "[ " ^ (tostr list) ^ " ]"

(* 
TODO:
Write an insertion sort function for lists of integers

for example,
sort [6;7;1] = [1;6;7]
*)

(* 
Hint: We encourage you to write the following helper function 

let rec insert (i: int) (list: int list): int list = failwith "unimplemented"

that takes a a number, an already sorted ls and returns a new sorted list with that number inserted
for example,
insert 5 [1;3;5;7] = [1;3;5;5;7]

You can  then call this helper function inside sort. 
*)


let rec inp (num: int) (list: int list): int list =
  match list with 
  | [] -> [num]
  | head::tail -> 
    if num >= head 
    then head:: (inp num tail)
    else num::head::tail 

let rec sort (ls: int list): int list =
  match ls with
  | [] -> []
  | head::tail -> inp head (sort tail)
