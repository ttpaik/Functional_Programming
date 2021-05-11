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

(* the type of a plymorphic tree *)
type 'a tree =
  | Leaf of 'a 
  | Node of 'a tree * 'a tree


(*
TODO: write a map function for trees:
For example,
map_tree (fun x -> x+1) (Node (Leaf 1, Leaf 2)) =  (Node (Leaf 2, Leaf 3))
map_tree (fun _ -> 0)  (Node (Node (Leaf true, Node (Leaf true, Leaf false)), Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
                       (Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Node (Node (Leaf 0   , Node (Leaf 0   , Leaf 0    )), Leaf 0    )))
*)
let rec map_tree (f: 'a -> 'b) (tree: 'a tree): 'b tree =
  match tree with
  | Leaf l -> Leaf (f l)
  | Node (ln, rn) -> Node(map_tree f ln, map_tree f rn)

(*
TODO: write a fold function for trees:
*)

let rec fold_tree (node: 'b -> 'b -> 'b)  (leaf: 'a -> 'b)  (tree: 'a tree): 'b  = 
  match tree with
  | Leaf l -> leaf l
  | Node (ln, rn) -> node (fold_tree node leaf ln) (fold_tree node leaf rn)



(*
TODO: sum the contents of an int tree
For example,
sum_ints (Node (Leaf 1, Leaf 2)) = 3
*)
let rec sum_ints (tree: int tree): int  = 
  match tree with
  | Leaf l -> l
  | Node (ln, rn) -> sum_ints ln + sum_ints rn

(*
TODO: find the size of the tree
For example,
tree_size (Leaf 1) = 1
tree_size (Node (Leaf 1, Leaf 2)) = 3
*)
let rec tree_size (tree: 'a tree): int  = 
  match tree with
  | Leaf l -> 1
  | Node (ln, rn) -> tree_size ln + tree_size rn + 1


(*
TODO: find the height of the tree
For example,
tree_height (Leaf 2) = 1
tree_height (Node ((Node (Leaf 1, (Node ((Node (Leaf 1, Leaf 2)), Leaf 2)))), Leaf 2)) = 5
*)
let rec tree_height (tree: 'a tree): int  = 
  match tree with
  | Leaf l -> 1
  | Node (ln, rn) -> max (tree_height ln) (tree_height rn) + 1

(*
TODO: write a function that takes a predicate on trees and retuns true if any subtree satisfies that predicate
For example,
tree_contains (Node (Leaf 1, Leaf 2)) (fun x -> match x with Leaf 2 -> true | _ -> false) = true
*)

let rec tree_contains (tree: 'a tree) (look_for: 'a tree -> bool): bool  = 
  match look_for(tree) with
  | true -> true 
  | false -> match tree with 
    | Leaf l -> look_for(Leaf l)
    | Node(ln, rn) -> tree_contains ln look_for || tree_contains rn look_for 


(*
TODO: write a function that shows bool trees :
For example,
show_bool_tree (Leaf true) = "true"
show_bool_tree (Node (Leaf true, Leaf false)) = "(true^false)" 
show_bool_tree  (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false))) =
    "((true^(true^false))^((true^(true^false))^false))" 
*)
let rec show_bool_tree (tree: bool tree) : string = 
  match tree with
  | Leaf l -> if l = true then "true" else "false"
  | Node (ln, rn) ->  "(" ^ show_bool_tree ln  ^ "^" ^ show_bool_tree rn ^ ")"


(* standard functions to convert between strin and char list *)
let explode s =
  let rec exp i l =
    if i < 0 then l else exp (i - 1) (s.[i] :: l) in
  exp (String.length s - 1) []

let implode l =
  let res = String.create (List.length l) in
  let rec imp i = function
    | [] -> res
    | c :: l -> res.[i] <- c; imp (i + 1) l in
  imp 0 l;;

(*
TODO: write a fubction that reads bool trees :
for all (finite) t : bool trees.
read_bool_tree t = Some (show_bool_tree t)
For example,
read_bool_tree "true" = Some (Leaf true)
read_bool_tree "false" = Some (Leaf false)
read_bool_tree "tralse" = None
read_bool_tree "(true^false)" = Some (Node (Leaf true, Leaf false))
read_bool_tree "((true^(true^false))^((true^(true^false))^false))" =
Some
 (Node (Node (Leaf true, Node (Leaf true, Leaf false)),
   Node (Node (Leaf true, Node (Leaf true, Leaf false)), Leaf false)))
*)

(* Hint 
   write a helper function 
   read_bool_prefix : (char list) -> ((bool * (char list)) option) 
   such that
   read_bool_prefix (explode "true???")       = Some (true, ['?'; '?'; '?'])
   read_bool_prefix (explode "false123")      = Some (false, ['1'; '2'; '3'])
   read_bool_prefix (explode "antythingales") = None
   read_bool_prefix []                        = None
   write a helper function 

   read_bool_tree_prefix (char list) -> ((bool tree * (char list)) option) 
   such that
   read_bool_tree_prefix [] = None
   read_bool_tree_prefix (explode "true???") = Some (Leaf true, ['?'; '?'; '?'])
   read_bool_tree_prefix (explode "(true^false)124") = Some (Node (Leaf true, Leaf false), ['1'; '2'; '4'])
   read_bool_tree_prefix (explode "(true^(true^false))aaa") = Some (Node (Leaf true, Node (Leaf true, Leaf false)), ['a'; 'a'; 'a'])
   read_bool_tree_prefix (explode "(true^(true^fa se))aaa") = None
*)

let gfc s =
  match s with
  | [] -> None
  | h::t -> Some (h, t)

let get_five s =
  match (gfc s) with
  | None -> None
  | Some (a, rest) -> match (gfc rest) with
    | None -> None
    | Some (b,rest2) -> match (gfc rest2) with
      | None -> None
      | Some (c, rest3) -> match (gfc rest3) with
        | None -> None
        | Some (d, rest4) -> match (gfc rest4) with
          | None -> None
          | Some (e, rest5) -> if [a;b;c;d] = ['t';'r';'u';'e'] then Some (implode ([a;b;c;d]), rest4) else Some (implode ([a;b;c;d;e]), rest5)

let rec read_bool_prefix clist : ((bool * (char list)) option) = 
  match get_five(clist) with
  | None -> None
  | Some (f, s) -> if f = Bytes.of_string "true" then Some (true, s) else if f = Bytes.of_string "false"
    then Some (false, s) else None

let getFirst t =
  match t with
  | [] -> ' '
  | h::t -> h

let getRest r =
  match r with
  | [] -> []
  | h::t -> t

let rec read_bool_tree_prefix inp : (bool tree * (char list)) option =
  match inp with
  | [] -> None
  | list -> let fin =
              if getFirst(list) = '(' then 
                match (read_bool_tree_prefix (getRest list)) with
                | None -> None
                | Some (a, b) ->
                  match b with
                  | [] -> None
                  | list2 -> if getFirst(list2) = ')' then Some (a, getRest(list2)) else None
              else 
                match (read_bool_prefix inp) with
                | None -> Some (Leaf false, inp)
                | Some (c, d) -> Some (Leaf c, d)  in
    match fin with
    | None -> None
    | Some (str, rest) -> 
      if getFirst(rest) = '^' then 
        match (read_bool_tree_prefix (getRest rest)) with
        | None -> None
        | Some (str2, rest2) -> Some (Node (str, str2), rest2) 
      else fin                 



let rec read_bool_tree (tree: string) : ((bool tree) option) = 
  if tree = "true" then Some (Leaf true) else if tree = "false" then Some (Leaf false) 
  else match read_bool_tree_prefix (explode tree) with
    | Some (tree, []) -> Some tree
    | _ -> None




(*
write a fubction that checks that parenthisis are balnaced:
Parenthisis are balenced if there are no parenthises
Parenthisis are balenced if ( and )  enclose a balenced parenthises
Parenthisis are balenced if balenced parenthises are ajacent to a balenced parenthisis
For example,
matching_parens "" = true
matching_parens "((((((((((()))))))))))" = true
matching_parens "()()()()()()" = true
matching_parens "(()())" = true
matching_parens "())(()" = false
*)


(* Hint 
   write mutually recirsive functions 
   matching_paren_prefix : (char list) -> ((char list) option)
   matching_parens_prefix : (char list) -> ((char list) option)
   the and keyword allows mutual recursion
   let rec matching_paren_prefix (ls: char list) : ((char list) option) = failwith "unimplemented"
   and matching_parens_prefix  (ls: char list) : ((char list) option) = failwith "unimplemented"
   such that
   matching_paren_prefix [] = None
   matching_paren_prefix (explode "(???") = None
   matching_paren_prefix (explode "()???") = Some ['?'; '?'; '?']
   matching_paren_prefix (explode "(((())))123") = Some ['1'; '2'; '3']
   matching_paren_prefix (explode "()()()") = Some ['('; ')'; '('; ')']
   matching_paren_prefix (explode "(()()())abc") = Some ['a'; 'b'; 'c']
   matching_parens_prefix [] = Some []
   matching_parens_prefix (explode "()()()") = Some ['('; ')'; '('; ')']
   matching_parens_prefix (explode ")aa") = Some [')'; 'a'; 'a']
*)


let replace list =
  list = []

let rec mp tree accum =
  match tree with
  | [] -> replace accum
  | any -> 
    if getFirst(any) = '(' then mp (getRest any) ('('::accum)
    else if getFirst(any) = ')' then match accum with
      | any2 -> (
          if getFirst(any2) = '(' then mp (getRest any) (getRest any2) 
          else false ) 
    else false 

let rec matching_parens (tree: string) : bool =
  mp (explode tree) []