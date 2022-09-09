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


let of_layout_annotation annot ~default =
  match annot with
  | None -> default
  | Some Builtin_attributes.Any         -> Any
  | Some Builtin_attributes.Value       -> Sort Value
  | Some Builtin_attributes.Void        -> Sort Void
  | Some Builtin_attributes.Immediate64 -> Immediate64
  | Some Builtin_attributes.Immediate   -> Immediate

(* let layout_of_attributes attrs =
 *   of_layout_annotation (Builtin_attributes.layout attrs) *)

let sort_to_string = function
  | Var _ -> "<unification variable>"
  | Value -> "value"
  | Void -> "void"

let to_string = function
  | Any -> "any"
  | Sort sort -> "sort " ^ sort_to_string sort
  | Immediate64 -> "immediate64"
  | Immediate -> "immediate"

module Violation = struct
  type nonrec t = Not_a_sublayout of t * t

  let report_with_offender ~offender ppf t =
    let pr fmt = Format.fprintf ppf fmt in
    match t with
    | Not_a_sublayout (l1,l2) ->
        pr "%t has layout %s, which is not a sublayout of %s." offender
          (to_string l1) (to_string l2)

  let report_with_name ~name ppf t =
    let pr fmt = Format.fprintf ppf fmt in
    let name = StringLabels.capitalize_ascii name in
    match t with
    | Not_a_sublayout (l1,l2) ->
        pr "%s has layout %s, which is not a sublayout of %s." name
          (to_string l1) (to_string l2)

  (* let report ppf t =
   *   let pr fmt = Format.fprintf ppf fmt in
   *   match t with
   *   | Not_a_sublayout (l1,l2) ->
   *       pr "Layout %s is not a sublayout of %s." name
   *  (Type_layout.to_string l1) (Type_layout.to_string l2) *)
end
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
let any_sort () = Sort (Var (ref None))
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

let rec sort_repr s =
  match s with
  | (Value | Void) -> s
  | Var r -> begin
      match !r with
      | Some s -> sort_repr s
      | None -> s
    end

let equal_sort s1 s2 =
  match sort_repr s1, sort_repr s2 with
  | Value, Value -> true
  | Void, Void -> true
  | (Var r, s) | (s, Var r) -> (r := Some s; true)
  | (Value | Void), _ -> false

let equal l1 l2 =
  match l1, l2 with
  | Any, Any
  | Immediate64, Immediate64
  | Immediate, Immediate -> true
  | Sort s1, Sort s2 -> equal_sort s1 s2
  | (Any | Immediate64 | Immediate | Sort _), _ -> false

let intersection l1 l2 =
  match l1, l2 with
  | (Any, l | l, Any) -> Ok l
  | ((Immediate64 | Immediate) as l, Sort s
    | Sort s, ((Immediate64 | Immediate) as l)) ->
    if equal_sort Value s then Ok l else Error (failwith "CJC XXX error message")
  | (Immediate, Immediate64 | Immediate64, Immediate)-> Ok Immediate64
  | _, _ ->
    if equal l1 l2 then Ok l2 else Error (failwith "CJC XXX error message")

let sublayout sub super =
  match sub, super with
  | _, Any -> Ok ()
  | (Immediate64 | Immediate), Sort s ->
      if equal_sort Value s then Ok ()
      else Error (Violation.Not_a_sublayout (sub,super))
  | Immediate, Immediate64 -> Ok ()
  | _, _ ->
      if equal sub super then Ok ()
      else Error (Violation.Not_a_sublayout (sub,super))
