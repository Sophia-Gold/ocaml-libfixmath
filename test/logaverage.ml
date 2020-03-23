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

open Libfixmath

(* log-average formulation of geometric mean *)
let mean (l: Fix16.t list) : Fix16.t =
  Fix16.exp (Fix16.mul
               (Fix16.div
                  (Fix16.from_int 1)
                  (Fix16.from_int (List.length l)))
               (List.fold_left Fix16.add (Fix16.from_int 0)
                  (List.map Fix16.ln l)))

let weighted_mean (l: (Fix16.t * Fix16.t) list) : Fix16.t =
  mean (List.map (fun (balance, weight) -> (Fix16.pow balance weight)) l)
