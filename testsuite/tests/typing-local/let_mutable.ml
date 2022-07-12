(* TEST
  flags = "-extension let_mutable"
   * expect *)

(* Test 1: basic usage in a for loop *)
let foo1 y =
  let mutable x = y in
  for i = 1 to 10 do
    x <- x + i
  done;
  x

let () = assert (Int.equal (foo1 0) 55)
let () = assert (Int.equal (foo1 42) 97)

[%%expect{|
val foo1 : int -> int = <fun>
|}]

(* Test 2: Reject use of mutable in closure. *)
let foo2 y =
  let mutable x = y in
  let add_55 () =
    for i = 1 to 10 do
      x <- x + i
    done;
    x
  in
  add_55

[%%expect{|
Line 5, characters 6-16:
5 |       x <- x + i
          ^^^^^^^^^^
Error: The variable x is mutable, so cannot be used inside a closure that might escape
|}]

(* Test 3: Rejected for same reason as test 2, but this one is actually safe and
   could be allowed with more sophisticated analysis in the future. *)
let foo3 y =
  let mutable x = y in
  let rec add_55 z =
    match z with
    | 0 -> x
    | z -> x <- x + z; add_55 (z-1)
  in
  add_55 10
[%%expect{|
Line 5, characters 11-12:
5 |     | 0 -> x
               ^
Error: The variable x is mutable, so cannot be used inside a closure that might escape
|}]

(* Test 4: Disallowed interactions with locals *)
let foo4_1 y =
  let mutable x = [] in
  for i = 1 to y do
    x <- local_ (i :: x)
  done;
  match x with
  | [] -> assert false
  | (x :: xs) -> x

[%%expect{|
Line 4, characters 9-24:
4 |     x <- local_ (i :: x)
             ^^^^^^^^^^^^^^^
Error: This value escapes its region
|}]


let foo4_2 y = (* Can't sneak local out of non-local for loop body region *)
  let mutable x = [] in
  let build_loop () =
    for i = 1 to y do local_
      x <- local_ (i :: x)
    done;
    match x with
    | [] -> assert false
    | (x :: xs) -> x
  in
  build_loop ()

[%%expect{|
Line 5, characters 6-26:
5 |       x <- local_ (i :: x)
          ^^^^^^^^^^^^^^^^^^^^
Error: The variable x is mutable, so cannot be used inside a closure that might escape
|}]


let foo4_3 y = (* Can't sneak local out of non-local while loop body region *)
  let mutable x = y in
  let i = ref 1 in
  while !i <= 10 do
    x <- (local_ (x + !i));
    i := !i + 1;
  done; x

[%%expect{|
Line 5, characters 9-26:
5 |     x <- (local_ (x + !i));
             ^^^^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

let foo4_4 y = (* Can't sneak localk out of non-local while cond region *)
  let mutable x = y in
  while x <- (local_ (x + 1)); x <= 100 do
    x <- x + x
  done; x

[%%expect{|
Line 3, characters 13-29:
3 |   while x <- (local_ (x + 1)); x <= 100 do
                 ^^^^^^^^^^^^^^^^
Error: This value escapes its region
|}]

(* Test 5: Allowed interactions with locals. *)
let foo5_1 y =  (* Assignment of local allowed in same scope *)
  let mutable x = [] in
  x <- (local_ (y :: x));
  x <- (local_ (y :: x));
  match x with
  | [] -> assert false
  | (x :: xs) -> x

let () = assert Int.(equal 42 (foo5_1 42))

let foo5_2 y =  (* Assignment of local works in _local_ for loop body region *)
  let mutable x = [] in
  for i = 1 to y do local_
    x <- local_ (i :: x)
  done;
  match x with
  | [] -> assert false
  | (x :: xs) -> x

let () = assert Int.(equal 42 (foo5_2 42))

let foo5_3 y = (* Assignment of local works in _local_ while body region *)
  let mutable x = y in
  let i = ref 1 in
  while !i <= 10 do local_
    x <- (local_ (x + !i));
    i := !i + 1;
  done; x

let foo5_4 y = (* Assign of local works in _local_ while cond region *)
  let mutable x = y in
  while local_ x <- (local_ (x + 1)); x <= 100 do
    x <- x + x
  done; x

[%%expect{|
val foo5_1 : 'a -> 'a = <fun>
val foo5_2 : int -> int = <fun>
val foo5_3 : int -> int = <fun>
val foo5_4 : int -> int = <fun>
|}]

(* Test 6: you can use mutable with and *)
let foo_6 () =
  let z = 3
  and mutable x = []
  in
  x <- z :: x;
  match x with
  | [] -> 0
  | z :: _ -> z
[%%expect{|
val foo_6 : unit -> int = <fun>
|}]

(* Test 7: mutable and rec don't mix
let foo_7 () =
  let rec z = 1 :: x
  and mutable x = []
  in
  match x with
  | [] -> 0
  | _ :: _ -> 1
[%%expect{|

  |}] *)
