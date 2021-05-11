(*
Honor code comes here: I pledge that this program represents my own
program code and that I have coded on my own. I received
help from no one in designing and debugging my program.
I have read the course syllabus of CS 320 and have read the sections on Collaboration
and Academic Misconduct. I also understand that I may be asked to meet the instructor
or the TF for a follow up interview on Zoom. I may be asked to explain my solution in person and
may also ask you to solve a related problem.

First Name: Taehyon
Last Name: Paik
BU ID: U58182574
*)

(*
Write a zip function that takes two lists of integers and combines them into a list of pairs of ints
If the two input list are of unequal lengths, return None
your method should be tail recursive.

For example,
safe_zip_int [1;2;3;5] [6;7;8;9] = Some [(1,6);(2,7);(3,8);(5,9)]
safe_zip_int [1] [2;4;6;8] = None
safe_zip_int (between 0 1000000) (between 0 1000000) does not stack overflow
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

let print_option_float f = 
  match f with
  | None -> print_string "None"
  | Some v -> print_tl(v)


let plo f = 
  match f with
  | None -> print_string "None"
  | Some v -> print_list(v)


let safe_zip_int (ls1: int list) (ls2: int list) : ((int * int) list) option = 
  let rec szi ls1 ls2 accum =
    match ls1, ls2 with
    | ([], []) -> Some (List.rev accum)
    | (_, []) -> None
    | ([],_) -> None
    | (h1::t1, h2::t2) -> szi t1 t2 ((h1,h2)::accum)
  in (szi ls1 ls2 [])


(*
Write a zip function that produces the ith Pell number:
https://en.wikipedia.org/wiki/Pell_number
https://oeis.org/A000129
your function should be tail recursive, but only needs to have the correct output up to integer overflow

pell 0 = 0
pell 1 = 1
pell 7 = 169
pell 1000000  does not stack overflow
*)


let rec pell (i: int) : int = 
  if i = 0 then 0 else if i = 1 then 1 else
    let rec pl2 i a1 a2 =
      if i = 0 then a1
      else pl2 (i - 1) a2 ((2* a2) + a1)
    in pl2 i 0 1



(* The nth Tetranacci number T(n) is mathematically defined as follows.
 *
 *      T(0) = 0
 *      T(1) = 1
 *      T(2) = 1
 *      T(3) = 2
 *      T(n) = T(n-1) + T(n-2) + T(n-3) + T(n-4)
 *
 * For more information, you may consult online sources.
 *
 *    https://en.wikipedia.org/wiki/Generalizations_of_Fibonacci_numbers
 *    https://mathworld.wolfram.com/TetranacciNumber.html
 *
 * Write a tail recursive function tetra that computes the nth Tetranacci
 * number efficiently.In particular, large inputs such as (tetra 1000000)
 * should neither cause stackoverflow nor timeout.
*)

let p3 num =
  let rec pl3 i w x y z =
    if i = 0 then w
    else if i = 1 then x
    else if i = 2 then y
    else if i = 3 then z
    else pl3 (i-1) x y z (w + x + y + z)
  in pl3 num 0 1 1 2

let tetra (n : int) : int = 
  if n = 0 then 0 else if n = 1 then 1 else if n = 2 then 1 else if n = 3 then 2 else if n = 4 then 4 else
    p3 n


(*
infinite precision natural numbers can be represented as lists of ints between 0 and 9

Write a function that takes an integer and represents it with a list of integers between 0 and 9 where the head ias the least signifigant digit.
If the input is negative return None

toDec 1234 = Some [4; 3; 2; 1]
toDec 0 = Some []
toDec -1234 = None
*)

(* Hint use 
   mod 10
   / 10
*)

let rec td i accum =
  match i with
  | 0 -> List.rev accum
  | _ -> td (i/10) ((i mod 10)::accum)

let rec toDec (i : int) : int list option =
  if i = 0 then Some []
  else if i < 0 then None
  else Some(td i [])


(*
Write a function that sums 2 natrual numbers as represented by a list of integers between 0 and 9 where the head ias the least signifigant digit.
Your function should be tail recursive

sum [4; 3; 2; 1] [1;0;1] = [5; 3; 3; 1]
sum [1] [9;9;9] = [0; 0; 0; 1]
sum [] [] = []
sum (nines 1000000) [1] does not stack overflow, when (nines 1000000) provides a list of 1000000 9s
*)

let rec sum (a : int list) (b : int list) : int list = 
  match a,b with
  | [], [] -> []
  | [], _ -> b
  | _, [] -> a 
  |first1::rest1, first2::rest2 -> if (first1+first2)>=10 then ((first1+first2) mod 10)::(sum (sum rest1 [1]) rest2)
    else (first1+first2)::(sum rest1 rest2)

(*
Write an infinite precision version of the pel function from before

pell2 0 = []
pell2 1 = [1]
pell2 7 = [9; 6; 1]
pell2 50 = [2; 2; 5; 3; 5; 1; 4; 2; 9; 2; 4; 6; 2; 5; 7; 6; 6; 8; 4]
*)
let rec toDec12 (i : int) : int list =
  if i <= 0 then []
  else td i []

let rec div li =
  match li with
  | [] -> []
  | h::[] -> if h > 10 then (toDec12 h) else List.rev(h::[])
  | h::t -> (h::t)

let rec pell2 (i: int) : int list = 
  if i = 0 then [] else if i = 1 then [1] else
    let rec peov a (a1: int list) (a2: int list) =
      if a = 0 then div(a1)
      else peov (a-1) (div(a2)) ((sum (sum (div(a2)) (div(a2))) (div(a1))))
    in peov i [0] [1]

let _= 
  print_list (pell2 6)