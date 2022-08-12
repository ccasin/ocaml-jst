(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*                   Jeremie Dimino, Jane Street Europe                   *)
(*                                                                        *)
(*   Copyright 2019 Jane Street Group LLC                                 *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

open Types

type t = layout

module Constant = struct
  type t =
    | Any
    | Value
    | Immediate64
    | Immediate
    | Void

  let rec constrain_sort_default_void = function
    | Types.Void -> Void
    | Types.Value -> Value
    | Types.Var r ->
      match !r with
      | Some sort -> constrain_sort_default_void sort
      | None -> (r := Some Types.Void; Void)

  let constrain_default_void = function
    | Types.Any -> Any
    | Types.Sort sort -> constrain_sort_default_void sort
    | Types.Immediate64 -> Immediate64
    | Types.Immediate -> Immediate
end

module Violation = struct
  type nonrec t = Not_a_sublayout of t * t
end

let of_layout_annotation annot =
  match annot with
  | None -> Any
  | Some Builtin_attributes.Any         -> Any
  | Some Builtin_attributes.Value       -> Sort Value
  | Some Builtin_attributes.Void        -> Sort Void
  | Some Builtin_attributes.Immediate64 -> Immediate64
  | Some Builtin_attributes.Immediate   -> Immediate

let sort_to_string = function
  | Var _ -> "<unification variable>"
  | Value -> "value"
  | Void -> "void"

let to_string = function
  | Any -> "any"
  | Sort sort -> sort_to_string sort
  | Immediate64 -> "immediate64"
  | Immediate -> "immediate"

(* let of_kind kind =
 *   match kind with
 *   | Type_abstract {layout} -> layout
 *   | Type_record _ -> Sort Value
 *   | Type_variant (cstrs,_) -> (* CJC XXX to fix or is this used? *)
 *      if List.exists (fun c -> c.cd_args <> Cstr_tuple []) cstrs
 *      then Sort Value
 *      else Immediate
 *   | Type_open -> Sort Value *)

let any = Any
let value = Sort Value
let immediate = Immediate
let immediate64 = Immediate64
let void = Sort Void

let layout_bound_of_record_representation = function
  | Record_regular -> value
  | Record_float -> value
  | Record_unboxed _ -> any
  | Record_inlined _ -> value
  | Record_extension _ -> value
  | Record_immediate _ -> immediate

let layout_bound_of_variant_representation = function
    Variant_regular -> value
  | Variant_unboxed -> any
  | Variant_immediate -> immediate

let layout_bound_of_kind = function
  | Type_open -> value
  | Type_record (_,rep) -> layout_bound_of_record_representation rep
  | Type_abstract { layout } -> layout
  | Type_variant (_, rep) -> layout_bound_of_variant_representation rep

let equal l1 l2 =
  match l1, l2 with
  | Any, Any
  | Immediate64, Immediate64
  | Immediate, Immediate
  | Sort Value, Sort Value
  | Sort Void, Sort Void -> true
  | Sort (Var _), _ | _, Sort (Var _) -> failwith "TODO" (* CJC XXX *)
  | _, _ -> false

let sublayout sub super =
  match sub, super with
  | _, Any
  | (Immediate64 | Immediate), Sort Value
  | Immediate, Immediate64 -> Ok ()
  | Sort (Var _), _ -> Ok () (* CJC XXX effect *)
  | l1, l2 when equal l1 l2 -> Ok ()
  | _ -> Error (Violation.Not_a_sublayout (sub,super))

