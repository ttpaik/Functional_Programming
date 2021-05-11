(*
Honor code comes here: 
I pledge that this program represents my own
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

type const =
  | I of int
  | B of bool
  | S of string
  | U of unit

type result =
  | NoError
  | DivError
  | StackError
  | Division_by_zero


type command =     
  | Push of const
  | Pop | Log | Swap | Add | Sub | Mul | Div | Rem | Neg


open Printf


let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

type 'a parser2 = Parser of (string -> ('a * string ) list)

let parse p s = 
  match p with 
    Parser f -> f s 

let charP  = 
  Parser (
    fun s ->
      match (explode s) with 
        []->[]
      |
        h::rest->[(h,implode rest)]
  )


let returnP a = 
  Parser 
    (
      fun s -> [a,s]
    )

let failP = 
  Parser
    (
      fun s->[]
    )




let (>>=) p f = 
  Parser (
    fun s ->
      match (parse p s ) with 
        []->[]   
      |
        (h,rest)::_->  let parser2 = f h in 
        match (parse parser2 rest) with 
          []->[]   
        |
          (h2,rest2)::_->[(h2,rest2)]
  )

let (let*) = (>>=)

let (<|>) a b = 
  Parser (
    fun s->  match (parse a s) with 
        []-> parse b s 
      |
        r->r
  )



let satcP (c:char)= 
  let* x = charP in
  if x=c then returnP c 
  else failP

let satsP s = 
  if s="" then failP else
    let rec asats (s:string)= 
      match (explode s) with 
        h::rest-> let* _ = satcP h in
        asats (implode rest)
      |
        []-> returnP([])
    in 
    asats (s:string)

let rec many0 p =
  (let* a = p in
   let* b = many0 p in
   returnP (a::b))
  <|>
  returnP []


let rec many1 p =
  let* a = p in
  let* b = many0 p in
  returnP (a::b)


let strP =
  Parser (
    fun s ->
      match (explode s) with
        [] -> []
      | h::[] -> [h, ""]
      | h::h1::rest -> 
        if  h != ';' && h1 != ' ' && h1 != '\n' && h1 != '\t' then [h,(implode (h1::rest))] 
        else if h == ';' && h1 != ' ' && h1 != '\n' && h1 != '\t' then [h, (implode (h1::rest))] 
        else if h1 == ' '  then  [h, (implode (h1::rest))] 
        else []
  )

let whitespaceP = 
  satcP ' ' <|> satcP '\t' <|> satcP '\n' <|> satcP ';'

let wsP =
  satsP ";\t" <|> satsP ";\n" <|> satsP "; "

let digitP = 
  let* x = charP in
  if '0' <= x && x <= '9' 
  then returnP x
  else failP

let natP = 
  (let* a = many1 digitP in 
   returnP (int_of_string (implode a)))

let integerP = 
  natP
  <|>
  (let* _ = satcP '-' in
   let* v = natP in
   returnP ((-1)*v)) 

let stringP =
  let* b = many1 strP in
  let* _ = wsP in
  returnP (implode b)


let unitP =
  let* _ = many0 whitespaceP in
  let* s = satsP "<unit>" in
  let* _ = many1 whitespaceP in
  returnP ()

let boolP =
  (let* _ = many0 whitespaceP in
   let* s = satsP "<true>" in
   let* _ = many1 whitespaceP in
   returnP true)
  <|>
  (let* _ = many0 whitespaceP in
   let* s1 = satsP "<false>" in
   let* _ = many1 whitespaceP in
   returnP false)


let pushP = 
  let* _ = many0 whitespaceP in
  let* _ = satsP "Push" in
  let* _ = many1 whitespaceP in
  (let* i = integerP in
   returnP (Push (I i))) 
  <|> 
  (let* b = boolP in 
   returnP (Push (B b)))
  <|>
  (let* u = unitP in
   returnP (Push (U u)))
  <|>
  (let* s = stringP in
   returnP (Push (S s)))

let addP = 
  let* _ = many0 whitespaceP in
  let* _ = satsP "Add" in
  let* _ = many1 whitespaceP in
  returnP (Add)

let popP =
  let* _ = many0 whitespaceP in
  let* _ = satsP "Pop" in
  let* _ = many1 whitespaceP in
  returnP (Pop)

let logP =
  let* _ = many0 whitespaceP in
  let* _ = satsP "Log" in
  let* _ = many1 whitespaceP in
  returnP (Log)

let swapP =
  let* _ = many0 whitespaceP in
  let* _ = satsP "Swap" in
  let* _ = many1 whitespaceP in
  returnP (Swap)

let subP =
  let* _ = many0 whitespaceP in
  let* _ = satsP "Sub" in
  let* _ = many1 whitespaceP in
  returnP (Sub)

let mulP =
  let* _ = many0 whitespaceP in
  let* _ = satsP "Mul" in
  let* _ = many1 whitespaceP in
  returnP (Mul)

let divP =
  let* _ = many0 whitespaceP in
  let* _ = satsP "Div" in
  let* _ = many1 whitespaceP in
  returnP (Div)

let remP =
  let* _ = many0 whitespaceP in
  let* _ = satsP "Rem" in
  let* _ = many1 whitespaceP in
  returnP (Rem)

let negP =
  let* _ = many0 whitespaceP in
  let* _ = satsP "Neg" in
  let* _ = many1 whitespaceP in
  returnP (Neg)





let commandP =
  pushP <|> addP <|> popP <|> logP <|> swapP <|> subP <|> mulP <|> divP <|> remP <|> negP



let commandsP = 
  many1 commandP


let hpush var stack err =
  var::stack, err

let hpop stack err =
  match (stack, err) with
  | [], _ -> [], 2
  | h::t,_ -> t, 0

let hswap stack err =
  match (stack, err) with
  | [],_ -> [],2
  | h::[],_ -> [],2
  | h::(h1::t),_ -> h1::(h::t),0

let hadd stack err =
  match  (stack, err) with
  | [],_ -> [],2
  | _::[], _ -> [],2
  | (I h)::(I h1)::t, _ -> (I (h + h1))::t,0
  | _::_,_ -> [],1

let hsub stack err =
  match  (stack, err) with
  | [],_ -> [],2
  | _::[], _ -> [],2
  | (I h)::(I h1)::t, _ -> (I (h - h1))::t,0
  | _::_,_ -> [],1


let hmul stack err =
  match  (stack, err) with
  | [],_ -> [],2
  | _::[],_ -> [],2
  | (I h)::(I h1)::t,_ -> (I (h * h1))::t,0
  | _::_,_ -> [],1

let hdiv stack err =
  match  (stack, err) with
  | [],_ -> [],2
  | _::[],_ -> [],2
  | (I h)::(I h1)::t,_ -> if (h1 == 0) then [],3 else (I (h / h1))::t,0
  | _::_,_ -> [],1


let hrem stack err =
  match  (stack, err) with
  | [],_ -> [],2
  | _::[],_ -> [],2
  | (I h)::(I h1)::t,_-> if (h1 == 0) then [],3 else (I (h mod h1))::t,0
  | _::_,_ -> [],1


let hneg stack err=
  match  (stack, err) with
  | [],_ -> [],2
  | (I h)::t,_ -> (I ((-1)*h) )::t,0
  | _::_,_ -> [],1


let hlog stack res =
  match  stack with
  | [] -> res
  | h::t-> h::res

let hlogst stack err =
  match  (stack, err) with
  | [],_ -> [],2
  | h::t,_ -> t,0


let rec tranconst l result =
  match l with
  | [] -> result
  | (I h)::t -> tranconst t (string_of_int h::result)
  | (B h)::t -> if h == false then tranconst t ("<false>"::result) else tranconst t ("<true>"::result)
  | (U h)::t -> tranconst t ("<unit>"::result)
  | (S h)::t -> tranconst t (h::result)

let rec exec parsestring res (stack, err)  =
  match parsestring with
  | [] -> tranconst res [], err
  | h::t -> if err == 0 then (match h with
      | Push var -> exec t res (hpush var stack 0) 
      | Pop -> exec t res (hpop stack err)  
      | Log -> exec t (hlog stack res) (hlogst stack err)  
      | Swap -> exec t res (hswap stack err)  
      | Add -> exec t res (hadd stack err) 
      | Sub -> exec t res (hsub stack err) 
      | Mul -> exec t res (hmul stack err) 
      | Div -> exec t res (hdiv stack err) 
      | Rem -> exec t res (hrem stack err) 
      | Neg -> exec t res (hneg stack err)  )
    else tranconst res [], err


let interpreter (s : string) : string list * int = 
  let parseOutput = (parse commandsP s) in
  let t = List.nth parseOutput 0 in
  match t with
  | [],_ -> [], 0
  | h,_ -> exec h [] ([],0)

let readlines (file : string) : string =
  let fp = open_in file in
  let rec loop () =
    match input_line fp with
    | s -> s ^ (loop ())
    | exception End_of_file -> ""
  in
  let res = loop () in
  let () = close_in fp in
  res


let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s
