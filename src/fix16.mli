(* Copyright 2020 Sophia Gold
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


type t

val min : t

val max : t


val from_int32 : int32 -> t

val to_int32 : t -> int32

val from_int : int -> t

val to_int : t -> int

val from_float : float -> t

val to_float : t -> float

val from_bigstring : Bigstring.t -> t

val to_bigstring : t -> Bigstring.t -> int -> unit


val add : t -> t -> t

val sub : t -> t -> t

val mul : t -> t -> t

val div : t -> t -> t


val exp : t -> t

val ln : t -> t


val log : t -> t -> t

val pow : t -> t -> t


val from_float_checked : float -> t option

val from_string_checked : string -> t option

val to_string : t -> string

val add_checked : t -> t -> t option

val sub_checked : t -> t -> t option

val mul_checked : t -> t -> t option

val div_checked : t -> t -> t option

val pow_checked : t -> t -> t option
