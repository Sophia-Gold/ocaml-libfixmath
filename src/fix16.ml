(* Copyright 2020 Sophia Gold.
 *
 * Permission is hereby granted, free of charge, to any person obtaining
 * a copy of this software and associated documentation files (the
 * "Software"), to deal in the Software without restriction, including
 * without limitation the rights to use, copy, modify, merge, publish,
 * distribute, sublicense, and/or sell copies of the Software, and to
 * permit persons to whom the Software is furnished to do so, subject to
 * the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
 * LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
 * OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE. *)


type t = int32

external fix16_min : unit -> (t [@unboxed]) =
  "ml_fix16_min" "ml_fix16_min_native" [@@noalloc]
let min = fix16_min ()

external fix16_max : unit -> (t [@unboxed]) =
  "ml_fix16_max" "ml_fix16_max_native" [@@noalloc]
let max = fix16_max ()


external from_int32 : int32 -> t =
  "ml_fix16_from_int32" "ml_fix16_from_int32_native" [@@noalloc] [@@unboxed]

external to_int32 : int32 -> t =
  "ml_fix16_to_int32" "ml_fix16_to_int32_native" [@@noalloc] [@@unboxed]

external from_int : int -> (t [@unboxed]) =
  "ml_fix16_from_int" "ml_fix16_from_int_native"

external to_int : (t [@unboxed]) -> int =
  "ml_fix16_to_int" "ml_fix16_to_int_native"

external from_float : float -> t =
  "ml_fix16_from_dbl" "ml_fix16_from_dbl_native" [@@noalloc] [@@unboxed]

external to_float : t -> float =
  "ml_fix16_to_dbl" "ml_fix16_to_dbl_native" [@@noalloc] [@@unboxed]

external from_string : Bigstring.t -> t =
  "ml_fix16_from_str" [@@noalloc]

external to_string : t -> Bigstring.t -> int -> unit =
  "ml_fix16_to_str" [@@noalloc]


external add : t -> t -> t =
  "ml_fix16_add" "ml_fix16_add_native" [@@noalloc] [@@unboxed]

external sub : t -> t -> t =
  "ml_fix16_sub" "ml_fix16_sub_native" [@@noalloc] [@@unboxed]

external mul : t -> t -> t =
  "ml_fix16_mul" "ml_fix16_mul_native" [@@noalloc] [@@unboxed]

external div : t -> t -> t =
  "ml_fix16_div" "ml_fix16_div_native" [@@noalloc] [@@unboxed]


external exp : t -> t =
  "ml_fix16_exp" "ml_fix16_exp_native" [@@noalloc] [@@unboxed]

external ln : t -> t =
  "ml_fix16_ln" "ml_fix16_ln_native" [@@noalloc] [@@unboxed]


external log : t -> t -> t =
  "ml_fix16_log" "ml_fix16_log_native" [@@noalloc] [@@unboxed]

external pow : t -> t -> t =
  "ml_fix16_pow" "ml_fix16_pow_native" [@@noalloc] [@@unboxed]


let from_float_checked a =
  if a <= (to_float min)
  then None
  else Some (from_float a)

let from_string_checked a =
  let f = from_string a in
  if (to_float f) <= (to_float min)
  then None
  else Some f

let add_checked a b =
  let result = add a b in
  if (to_float result) <= (to_float min)
  then None
  else Some result

let sub_checked a b =
  let result = sub a b in
  if (to_float result) <= (to_float min)
  then None
  else Some result

let mul_checked a b =
  let result = mul a b in
  if (to_float result) <= (to_float min)
  then None
  else Some result

let div_checked a b =
  let result = div a b in
  if (to_float result) <= (to_float min)
  then None
  else Some result
