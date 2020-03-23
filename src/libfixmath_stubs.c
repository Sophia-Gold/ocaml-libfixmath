/* Copyright 2020 Sophia Gold.
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
 * WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 */

#include <caml/alloc.h>
#include <caml/bigarray.h>
#include "fixmath.h"

#define FIXMATH_NO_OVERFLOW

CAMLprim int32_t ml_fix16_min_native() {
  return fix16_minimum;
}
CAMLprim value ml_fix16_min() {
  return caml_copy_int32(ml_fix16_min_native());
}

CAMLprim int32_t ml_fix16_max_native() {
  return fix16_maximum;
}
CAMLprim value ml_fix16_max() {
  return caml_copy_int32(ml_fix16_max_native());
}


CAMLprim int32_t ml_fix16_from_int32_native(int32_t arg) {
  return fix16_from_int(arg);
}
CAMLprim value ml_fix16_from_int32(value arg) {
  return caml_copy_int32(ml_fix16_from_int32_native(Int32_val(arg)));
}

CAMLprim int32_t ml_fix16_to_int32_native(int32_t arg) {
  return fix16_to_int(arg);
}
CAMLprim value ml_fix16_to_int32(value arg) {
  return caml_copy_int32(ml_fix16_to_int32_native(Int32_val(arg)));
}


CAMLprim int32_t ml_fix16_from_int_native(value arg) {
  return fix16_from_int(Int_val(arg));
}
CAMLprim value ml_fix16_from_int(value arg) {
  return caml_copy_int32(ml_fix16_from_int_native(arg));
}

CAMLprim value ml_fix16_to_int_native(int32_t arg) {
  return Val_int(fix16_to_int(arg));
}
CAMLprim value ml_fix16_to_int(value arg) {
  return ml_fix16_to_int_native(Int32_val(arg));
}


CAMLprim int32_t ml_fix16_from_dbl_native(double arg) {
  return fix16_from_dbl(arg);
}
CAMLprim value ml_fix16_from_dbl(value arg) {
  return caml_copy_int32(ml_fix16_from_dbl_native(Double_val(arg)));
}

CAMLprim double ml_fix16_to_dbl_native(int32_t arg) {
  return fix16_to_dbl(arg);
}
CAMLprim value ml_fix16_to_dbl(value arg) {
  return caml_copy_double(ml_fix16_to_dbl_native(Int32_val(arg)));
}

CAMLprim value ml_fix16_from_str(value buf) {
  return caml_copy_int32(fix16_from_str(Caml_ba_data_val(buf)));
}

CAMLprim value ml_fix16_to_str(value arg, value buf, value len) {
  fix16_to_str(Int32_val(arg),
	       Caml_ba_data_val(buf),
	       Int_val(len));
  return Val_unit;
}


CAMLprim int32_t ml_fix16_add_native(int32_t arg0, int32_t arg1) {
  return fix16_add(arg0, arg1);
}
CAMLprim value ml_fix16_add(value arg0, value arg1) {
  return caml_copy_int32(ml_fix16_add_native(Int32_val(arg0),
					     Int32_val(arg1)));
}

CAMLprim int32_t ml_fix16_sub_native(int32_t arg0, int32_t arg1) {
  return fix16_sub(arg0, arg1);
}
CAMLprim value ml_fix16_sub(value arg0, value arg1) {
  return caml_copy_int32(ml_fix16_sub_native(Int32_val(arg0),
					     Int32_val(arg1)));
}

CAMLprim int32_t ml_fix16_mul_native(int32_t arg0, int32_t arg1) {
  return fix16_mul(arg0, arg1);
}
CAMLprim value ml_fix16_mul(value arg0, value arg1) {
  return caml_copy_int32(ml_fix16_mul_native(Int32_val(arg0),
					     Int32_val(arg1)));
}

CAMLprim int32_t ml_fix16_div_native(int32_t arg0, int32_t arg1) {
  return fix16_div(arg0, arg1);
}
CAMLprim value ml_fix16_div(value arg0, value arg1) {
  return caml_copy_int32(ml_fix16_div_native(Int32_val(arg0),
					     Int32_val(arg1)));
}


CAMLprim int32_t ml_fix16_exp_native(int32_t arg) {
  return fix16_exp(arg);
}
CAMLprim value ml_fix16_exp(value arg) {
  return caml_copy_int32(ml_fix16_exp_native(Int32_val(arg)));
}


CAMLprim int32_t ml_fix16_ln_native(int32_t arg) {
  return fix16_log(arg);
}
CAMLprim value ml_fix16_ln(value arg) {
  return caml_copy_int32(ml_fix16_ln_native(Int32_val(arg)));
}


CAMLprim int32_t ml_fix16_log_native(int32_t x, int32_t base) {
  return fix16_div(fix16_log(x),
		   fix16_log(base));
}
CAMLprim value ml_fix16_log(value x, value base)
{
  return caml_copy_int32(ml_fix16_log_native(Int32_val(x),
					     Int32_val(base)));
}

CAMLprim int32_t ml_fix16_pow_native(int32_t n, int32_t exp) {
  if (exp == 0)
    return fix16_one;
  if (n < 0)
    return 0;
  return fix16_exp(fix16_mul(fix16_log(n),
			     exp));
}
CAMLprim value ml_fix16_pow(value n, value exp)
{
  return caml_copy_int32(ml_fix16_pow_native(Int32_val(n),
					     Int32_val(exp)));
}
