(* TEST
   * expect
*)
type t_any   [@@any]
type t_value [@@value]
type t_imm   [@@immediate]
type t_imm64 [@@immediate64]
type t_void  [@@void];;
[%%expect{|
type t_any
type t_value
type t_imm [@@immediate]
type t_imm64 [@@immediate64]
type t_void
|}];;

(* Test 1: Reject non-value function arg/returns *)
module type S = sig
  val f : t_any -> int
end;;
[%%expect {|
Line 2, characters 10-15:
2 |   val f : t_any -> int
              ^^^^^
Error: Function argument types must have layout value.
        t_any has layout any, which is not a sublayout of value.
|}]

module type S = sig
  val f : int -> t_void
end;;
[%%expect {|
Line 2, characters 17-23:
2 |   val f : int -> t_void
                     ^^^^^^
Error: Function return types must have layout value.
        t_void has layout void, which is not a sublayout of value.
|}];;

(* Test 2: Permit value function arg/returns *)
module type S = sig
  val f1 : t_value -> t_value
  val f2 : t_imm -> t_imm64
end;;

[%%expect{|
module type S = sig val f1 : t_value -> t_value val f2 : t_imm -> t_imm64 end
|}];;

(* Test 3: basic annotated parameters *)
type 'a [@immediate] imm_id = 'a

[%%expect{|
type 'a imm_id = 'a
|}];;

type my_int = int imm_id
let plus_3 (x : my_int) = x + 3
let plus_3' (x : int imm_id) = x + 3;;

[%%expect{|
type my_int = int imm_id
val plus_3 : my_int -> int = <fun>
val plus_3' : int imm_id -> int = <fun>
|}];;

let string_id (x : string imm_id) = x;;
[%%expect{|
Line 1, characters 19-25:
1 | let string_id (x : string imm_id) = x;;
                       ^^^^^^
Error: This type string should be an instance of type 'a
       string has layout value, which is not a sublayout of immediate.
|}];;

(* Test 4: parameters and recursion *)
type 'a [@immediate] t4
and s4 = string t4;;

[%%expect{|
Line 2, characters 9-15:
2 | and s4 = string t4;;
             ^^^^^^
Error: This type string should be an instance of type 'a
       string has layout value, which is not a sublayout of immediate.
|}];;

type s4 = string t4
and 'a [@immediate] t4;;

[%%expect{|
Line 1, characters 10-16:
1 | type s4 = string t4
              ^^^^^^
Error: This type string should be an instance of type 'a
       string has layout value, which is not a sublayout of immediate.
|}]

type 'a [@any] t4 = 'a
and s4 = string t4;;
[%%expect{|
type 'a t4 = 'a
and s4 = string t4
|}];;

type s4 = string t4
and 'a [@any] t4;;
[%%expect{|
type s4 = string t4
and 'a t4
|}];;


(* CJC XXX bug.  This is caused by the choice of "value" in the Ptyp_var case of transl_type_aux. *)
type 'a [@void] baz = 'a;;

[%%expect{|
Line 1, characters 22-24:
1 | type 'a [@void] baz = 'a;;
                          ^^
Error: This type 'a should be an instance of type 'a0
       'a has layout void, which does not overlap with value.
|}]
