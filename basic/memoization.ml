(* Memoization 

   (int -> int) -> (int -> int)


   fib(n) = fib (n-1) + fib (n-2)
   where 
   n = 0 is 0
   n = 1 is 1
*)
let env = ref []


let memoize f : (int -> int) =
  let rec fetch x env =
    match env with
    | [] -> false, None
    | (key, value)::rest -> if x = key then (true, Some value) else fetch x rest
  in

  let memoizeFunc x : int =
    match (fetch x !env) with
    | true, Some value -> value
    | _,_ -> let fx = f x in
      env := (x, fx)::!env;
      fx in memoizeFunc

let rec fib x =
  match x with
  | 0 -> 0
  | 1 -> 1
  | n -> memoize fib (x-1) + memoize fib (x-2)

let time f x =
  let t1 = Sys.time() in
  let fx = f x in
  Printf.printf "Execute time is : %fs\n" (Sys.time() -. t1);
  Printf.printf "Function value is : %i\n" fx

let _ = 
  let memFib = memoize fib in
  time memFib 1000

let f x =
  match x with
  | (i:int), b -> 

    let _ =
      print_int (f (0, true))