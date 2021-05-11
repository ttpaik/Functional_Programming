open Printf

(* language syntax *)

type const =
  | I of int
  | B of bool
  | S of string
  | N of string
  | U

type command =
  (* part 1 *)
  | Push of const | Pop | Swap | Log | Add | Sub | Mul | Div | Rem | Neg
  (* part 2 *)
  | Cat | And | Or | Not | Eq | Lte | Lt | Gte | Gt | Let | Ask 
  | Block of command list | Ifte of (command list * command list)
  (* part 3 *)
  | DefFun of (const * const) * command list
  | Call | Throw | TryCatch of (command list * command list)

type prog = command list

(* parser util *)

type 'a parser = char list -> ('a * char list) option

let explode s =
  List.of_seq (String.to_seq s)

let implode ls =
  String.of_seq (List.to_seq ls)

let parse (s : string) (p : 'a parser) : ('a * char list) option =
  p (explode s)

let read : char parser =
  fun ls ->
  match ls with
  | x :: ls -> Some (x, ls)
  | _ -> None

let satisfy (f : char -> bool) : char parser =
  fun ls ->
  match ls with
  | x :: ls ->
    if f x then Some (x, ls)
    else None
  | _ -> None

let char (c : char) : char parser =
  satisfy (fun x -> x = c)

let seq (p1 : 'a parser) (p2 : 'b parser) : 'b parser =
  fun ls ->
  match p1 ls with
  | Some (_, ls) -> p2 ls
  | None -> None

let (>>) = seq

let seq' (p1 : 'a parser) (p2 : 'b parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (_, ls) -> Some (x, ls)
     | None -> None)
  | None -> None

let (<<) = seq'

let both (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls) ->
    (match p2 ls with
     | Some (y, ls) -> Some ((x, y), ls)
     | None -> None)
  | None -> None

let (+++) = both

let disj (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  fun ls ->
  match p1 ls with
  | Some (x, ls)  -> Some (x, ls)
  | None -> p2 ls

let (<|>) = disj

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  fun ls ->
  match p ls with
  | Some (x, ls) -> Some (f x, ls)
  | None -> None

let (>|=) = fun p f -> map f p
let (>|) = fun p c -> map (fun _ -> c) p

let rec many (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1 (p : 'a parser) : ('a list) parser =
  fun ls ->
  match p ls with
  | Some (x, ls) ->
    (match many p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let rec many' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> Some ([], ls)

let rec many1' (p : unit -> 'a parser) : ('a list) parser =
  fun ls ->
  match p () ls with
  | Some (x, ls) ->
    (match many' p ls with
     | Some (xs, ls) -> Some (x :: xs, ls)
     | None -> Some (x :: [], ls))
  | None -> None

let literal (s : string) : unit parser =
  fun ls ->
  let cs = explode s in
  let rec loop cs ls =
    match cs, ls with
    | [], _ -> Some ((), ls)
    | c :: cs, x :: xs ->
      if x = c
      then loop cs xs
      else None
    | _ -> None
  in loop cs ls

let whitespace : unit parser =
  fun ls ->
  match ls with
  | c :: ls ->
    if String.contains " \012\n\r\t" c
    then Some ((), ls)
    else None
  | _ -> None

let ws : unit parser =
  (many whitespace) >| ()

let ws1 : unit parser =
  (many1 whitespace) >| ()

let bool : bool parser =
  (literal "<true>"  >| true ) <|>
  (literal "<false>" >| false)

let digit : char parser =
  satisfy (fun x -> '0' <= x && x <= '9')

let natural : int parser =
  fun ls ->
  match many1 digit ls with
  | Some (xs, ls) ->
    Some (int_of_string (implode xs), ls)
  | _ -> None

let integer : int parser =
  ((char '-') >> ws >> (natural >|= (fun x -> -x))) <|> natural

let alpha : char parser =
  satisfy (fun x -> ('a' <= x && x <= 'z') ||
                    ('A' <= x && x <= 'Z'))

let string : string parser =
  (char '"') >> many (satisfy (fun x -> x <> '"')) << (char '"') >|= (fun cs -> implode cs)

let name : string parser =
  alpha +++ (many (alpha <|>
                   digit <|>
                   (char '_') <|>
                   (char '\''))) >|=
  (fun (c, cs) -> implode (c :: cs))

let unit : unit parser =
  (literal "<unit>") >| ()

(* parser *)

let sep : unit parser =
  char ';' >> ws >| ()

let keyword (s : string) : unit parser =
  (literal s) >> ws >| ()

let const =
  (integer >|= (fun x -> I x)) <|>
  (bool    >|= (fun x -> B x)) <|>
  (string  >|= (fun x -> S x)) <|>
  (name    >|= (fun x -> N x)) <|>
  (unit    >| U)

let rec command () =
  (* part 1 *)
  ((keyword "Push") >> (const) << sep >|= (fun x -> Push x)) <|>
  ((keyword "Pop")             << sep >| Pop)    <|>
  ((keyword "Swap")            << sep >| Swap)   <|>
  ((keyword "Log")             << sep >| Log)    <|>
  ((keyword "Add")             << sep >| Add)    <|>
  ((keyword "Sub")             << sep >| Sub)    <|>
  ((keyword "Mul")             << sep >| Mul)    <|>
  ((keyword "Div")             << sep >| Div)    <|>
  ((keyword "Rem")             << sep >| Rem)    <|>
  ((keyword "Neg")             << sep >| Neg)    <|>
  (* part 2 *)
  ((keyword "Cat")             << sep >| Cat)    <|>
  ((keyword "And")             << sep >| And)    <|>
  ((keyword "Or")              << sep >| Or)     <|>
  ((keyword "Not")             << sep >| Not)    <|>
  ((keyword "Eq")              << sep >| Eq)     <|>
  ((keyword "Lte")             << sep >| Lte)    <|>
  ((keyword "Lt")              << sep >| Lt)     <|>
  ((keyword "Gte")             << sep >| Gte)    <|>
  ((keyword "Gt")              << sep >| Gt)     <|>
  ((keyword "Let")             << sep >| Let)    <|>
  ((keyword "Ask")             << sep >| Ask)    <|>
  ((block ())                  << sep)           <|>
  ((ifte ())                   << sep)           <|>
  (* part 3 *)
  ((keyword "Call")            << sep >| Call)   <|>
  ((keyword "Throw")           << sep >| Throw)  <|>
  ((deffun ())                 << sep)           <|>
  ((trycatch ())               << sep)          

and block () =
  (keyword "Begin") >> many' command << (keyword "End") >|=
  (fun cmds -> Block cmds)

and ifte () =
  ((keyword "If") >> many' command) +++
  ((keyword "Else") >> many' command) << (keyword "End") >|=
  (fun (cmds1, cmds2) -> Ifte (cmds1, cmds2))

and deffun () =
  (((keyword "DefFun") >> const) +++ (ws >> const)) +++
  (ws >> many' command << (keyword "End")) >|= 
  (fun ((con1, con2), cmd) -> DefFun ((con1, con2),cmd))

and trycatch () = 
  ((keyword "Try") >> many' command) +++
  ((keyword "Catch") >> many' command) << (keyword "End") >|=
  (fun (cmd1, cmd2) -> TryCatch (cmd1, cmd2))

let parser = ws >> many' command

(* semantics *)

(* language value *)
type value =
  | I_val of int
  | B_val of bool
  | S_val of string
  | N_val of string
  | U_val
  | Func  of (string * string * command list * env)

and result =
  | Ok of value list
  | TypeError
  | StackError
  | DivError
  | UnboundError
  | UDError of int

and env = (string * value) list

and stack = value list

let lookup (e : env) (name : string) : value option =
  List.assoc_opt name e

let put (e : env) (name : string) (v : value): env =
  (name,v) :: e

let to_string_value v =
  match v with
  | I_val x -> sprintf "%d" x
  | B_val x -> sprintf "<%b>" x
  | S_val x -> sprintf "\"%s\"" x
  | N_val x -> x
  | U_val   -> "<unit>"
  | Func  x -> "<fun>"

let to_int_result (r : result) : int =
  match r with
  | Ok _          -> 0
  | TypeError     -> 1
  | StackError    -> 2
  | DivError      -> 3
  | UnboundError  -> 4
  | UDError i     -> i


let rec run (p : prog) (st : stack) (e : env) (log : string list) :
  string list * result =
  match p with
  (* part 1 *)
  | Push cst :: rest ->
    (match cst with
     | I v -> run rest (I_val v :: st) e log
     | B v -> run rest (B_val v :: st) e log
     | S v -> run rest (S_val v :: st) e log
     | N v -> run rest (N_val v :: st) e log
     | U   -> run rest (U_val   :: st) e log)
  | Pop :: rest ->
    (match st with
     | _ :: st -> run rest st e log
     | _ -> log, StackError)
  | Swap :: rest ->
    (match st with
     | x :: y :: st -> run rest (y :: x :: st) e log
     | _ -> log, StackError)
  | Log :: rest ->
    (match st with
     | x :: st -> run rest st e (to_string_value x :: log)
     | _ -> log, StackError)
  | Add :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (I_val (x + y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Sub :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (I_val (x - y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Mul :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (I_val (x * y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Div :: rest ->
    (match st with
     | I_val _ :: I_val 0 :: st -> log, DivError
     | I_val x :: I_val y :: st ->
       run rest (I_val (x / y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Rem :: rest ->
    (match st with
     | I_val _ :: I_val 0 :: st -> log, DivError
     | I_val x :: I_val y :: st ->
       run rest (I_val (x mod y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Neg :: rest ->
    (match st with
     | I_val x :: st ->
       run rest (I_val (-x) :: st) e log
     | _ :: st -> log, TypeError
     | _ -> log, StackError)

  (* part 2 *)
  | Cat :: rest ->
    (match st with
     | S_val x :: S_val y :: st ->
       run rest (S_val (x ^ y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | And :: rest ->
    (match st with
     | B_val x :: B_val y :: st ->
       run rest (B_val (x && y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Or :: rest ->
    (match st with
     | B_val x :: B_val y :: st ->
       run rest (B_val (x || y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Not :: rest ->
    (match st with
     | B_val x :: st ->
       run rest (B_val (not x) :: st) e log
     | _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Eq :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (B_val (x = y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Lte :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (B_val (x <= y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Lt :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (B_val (x < y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Gte :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (B_val (x >= y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Gt :: rest ->
    (match st with
     | I_val x :: I_val y :: st ->
       run rest (B_val (x > y) :: st) e log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Let :: rest ->
    (match st with
     | N_val x :: y :: st ->
       run rest st (put e x y) log
     | _ :: _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Ask :: rest ->
    (match st with
     | N_val x :: st ->
       (match lookup e x with
        | Some v -> run rest (v :: st) e log
        | None -> log, UnboundError)
     | _ :: st -> log, TypeError
     | _ -> log, StackError)
  | Block cmds :: rest ->
    (match run cmds [] e log with
     | (log, Ok (v :: _)) -> run rest (v :: st) e log
     | (log, Ok _) -> log, StackError
     | error -> error)
  | Ifte (cmds1, cmds2) :: rest ->
    (match st with
     | B_val b :: st ->
       if b
       then run (cmds1 @ rest) st e log
       else run (cmds2 @ rest) st e log
     | _ :: st -> log, TypeError
     | _ -> log, StackError)

  (* part 3 *)
  | DefFun ((con1, con2), cmd) :: rest ->
    (match con1, con2 with
     | (N c1), (N c2) -> run rest [] (put e c1 (Func (c1, c2, cmd, e))) []
     | _ -> run rest st e log)
  | Call :: rest ->
    (match st with 
     | h::(Func (c1, c2, cmd, e1))::sta ->
       (match run cmd sta ((c2,h) :: ((c1,(Func (c1, c2, cmd, e1))) :: e1)) [] with 
        | (log1, Ok (v :: _)) -> run rest (v :: sta) e (log1@log)
        | (log1, Ok _) -> log1@log, TypeError
        | (log1, err) -> (match lookup e "printer" with
            | None -> log1@log, err
            | Some _ -> log1@log, Ok st))
     | h::_::sta -> log, TypeError
     | _ ::[] -> log, StackError
     | _ -> log, StackError)   
  | Throw :: rest ->
    (match st with
     | I_val x :: st -> log, UDError x
     | _ :: st -> log, TypeError
     | _ -> log, StackError)
  | TryCatch (cmd1, cmd2) :: rest ->
    (match run cmd1 st e log with
     | (log, Ok sta) -> run rest sta e log
     | (log, UDError i) -> (match run cmd2 (I_val i::st) e log with 
         | (log, Ok sta) -> run rest sta e log
         | log, err -> log, err)
     | log, err -> (match run cmd2 (I_val (to_int_result err)::st) e log with 
         | (log, Ok sta) -> run rest sta e log
         | log, err -> log, err))
  | [] -> log, Ok st

(* putting it all together *)

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

let interpreter (s : string) : string list * int =
  match parse s parser with
  | Some (prog, []) ->
    let (log, ret) = run prog [] [] [] in
    (List.rev log, to_int_result ret)
  | _ -> failwith "invalid source"

let runfile (file : string) : string list * int =
  let s = readlines file in
  interpreter s
