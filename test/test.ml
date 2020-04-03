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


open Libfixmath

let convert_int () =
  Alcotest.(check int)
    "same"
    42
    (Fix16.to_int (Fix16.from_int 42))

let convert_int32 () =
  Alcotest.(check int32)
    "same"
    42l
    (Fix16.to_int32 (Fix16.from_int32 42l))

let convert_int_neg () =
  Alcotest.(check int)
    "same"
    (-42)
    (Fix16.to_int (Fix16.from_int (-42)))


exception Imprecise of float * float
exception Undetected_Overflow of float * float
let max_delta = Fix16.to_float (Fix16.div (Fix16.from_float 1.) Fix16.max)

let testcases = [
    (* Small numbers *)
    0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9.; 10.;
    -1.; -2.; -3.; -4.; -5.; -6.; -7.; -8.; -9.; -10.;

    (* Integer numbers *)
    0x10000.; -0x10000.; 0x20000.; -0x20000.; 0x30000.; -0x30000.;
    0x40000.; -0x40000.; 0x50000.; -0x50000.; 0x60000.; -0x60000.;

    (* Fractions (1/2.; 1/4.; 1/8) *)
    0x8000.; -0x8000.; 0x4000.; -0x4000.; 0x2000.; -0x2000.;

    (* Problematic carry *)
    0xFFFF.; -0xFFFF.; 0x1FFFF.; -0x1FFFF.; 0x3FFFF.; -0x3FFFF.;

    (* Smallest and largest values *)
    0x7FFFFFFF.; 0x80000000.;

    (* Large random numbers *)
    831858892.; 574794913.; 2147272293.; -469161054.; -961611615.;
    1841960234.; 1992698389.; 520485404.; 560523116.; -2094993050.;
    -876897543.; -67813629.; 2146227091.; 509861939.; -1073573657.;

    (* Small random numbers *)
    -14985.; 30520.; -83587.; 41129.; 42137.; 58537.; -2259.; 84142.;
    -28283.; 90914.; 19865.; 33191.; 81844.; -66273.; -63215.; -44459.;
    -11326.; 84295.; 47515.; -39324.;

    (* Tiny random numbers *)
    -171.; -359.; 491.; 844.; 158.; -413.; -422.; -737.; -575.; -330.;
    -376.; 435.; -311.; 116.; 715.; -1024.; -487.; 59.; 724.; 993.
  ]


let add () =
  List.iter (fun a ->
      List.iter (fun b ->
          match Fix16.add_checked (Fix16.from_float a) (Fix16.from_float b) with
          | None -> ()
          | Some result ->
             let a_float = Fix16.to_float (Fix16.from_float a) in
             let b_float = Fix16.to_float (Fix16.from_float b) in
             let fresult = a_float +. b_float in
             if (fresult > Fix16.to_float Fix16.max)
                || (fresult < Fix16.to_float Fix16.min)
             then raise (Undetected_Overflow (a_float, b_float))
             else let delta = Float.abs ((Fix16.to_float result) -. fresult) in
                  if delta > max_delta
             then raise (Imprecise (fresult, delta))
             else ()
        ) testcases
    ) testcases


let sub () =
  List.iter (fun a ->
      List.iter (fun b ->
          match Fix16.sub_checked (Fix16.from_float a) (Fix16.from_float b) with
          | None -> ()
          | Some result ->
             let a_float = Fix16.to_float (Fix16.from_float a) in
             let b_float = Fix16.to_float (Fix16.from_float b) in
             let fresult = a_float -. b_float in
             if (fresult > Fix16.to_float Fix16.max)
                || (fresult < Fix16.to_float Fix16.min)
             then raise (Undetected_Overflow (a_float, b_float))
             else let delta = Float.abs ((Fix16.to_float result) -. fresult) in
                  if delta > max_delta
                  then raise (Imprecise (fresult, delta))
                  else ()
        ) testcases
    ) testcases


let mul0 () =
  Alcotest.(check int)
    "same"
    25
    (Fix16.to_int (Fix16.mul (Fix16.from_int 5) (Fix16.from_int 5)))

let mul1 () =
  Alcotest.(check int)
    "same"
    (-25)
    (Fix16.to_int (Fix16.mul (Fix16.from_int (-5)) (Fix16.from_int 5)))

let mul2 () =
  Alcotest.(check int)
    "same"
    25
    (Fix16.to_int (Fix16.mul (Fix16.from_int (-5)) (Fix16.from_int (-5))))

let mul3 () =
  Alcotest.(check int)
    "same"
    (-25)
    (Fix16.to_int (Fix16.mul (Fix16.from_int 5) (Fix16.from_int (-5))))

let mul () =
  List.iter (fun a ->
      List.iter (fun b ->
          match Fix16.mul_checked (Fix16.from_float a) (Fix16.from_float b) with
          | None -> ()
          | Some result ->
             let a_float = Fix16.to_float (Fix16.from_float a) in
             let b_float = Fix16.to_float (Fix16.from_float b) in
             let fresult = a_float *. b_float in
             if (fresult > Fix16.to_float Fix16.max)
                || (fresult < Fix16.to_float Fix16.min)
             then raise (Undetected_Overflow (a_float, b_float))
             else let delta = Float.abs ((Fix16.to_float result) -. fresult) in
                  if delta > max_delta
             then raise (Imprecise (fresult, delta))
             else ()
        ) testcases
    ) testcases


let div0 () =
  Alcotest.(check int)
    "same"
    3
    (Fix16.to_int (Fix16.div (Fix16.from_int 15) (Fix16.from_int 5)))

let div1 () =
  Alcotest.(check int)
    "same"
    (-3)
    (Fix16.to_int (Fix16.div (Fix16.from_int (-15)) (Fix16.from_int 5)))

let div2 () =
  Alcotest.(check int)
    "same"
    3
    (Fix16.to_int (Fix16.div (Fix16.from_int (-15)) (Fix16.from_int (-5))))

let div3 () =
  Alcotest.(check int)
    "same"
    (-3)
    (Fix16.to_int (Fix16.div (Fix16.from_int 15) (Fix16.from_int (-5))))

let div () =
  List.iter (fun a ->
      List.iter (fun b ->
          match Fix16.div_checked (Fix16.from_float a) (Fix16.from_float b) with
          | None -> ()
          | Some result ->
             let a_float = Fix16.to_float (Fix16.from_float a) in
             let b_float = Fix16.to_float (Fix16.from_float b) in
             let fresult = a_float /. b_float in
             if (fresult > Fix16.to_float Fix16.max)
                || (fresult < Fix16.to_float Fix16.min)
             then raise (Undetected_Overflow (a_float, b_float))
             else let delta = Float.abs ((Fix16.to_float result) -. fresult) in
                  if delta > max_delta
                  then raise (Imprecise (fresult, delta))
                  else ()
        ) testcases
    ) testcases


let exp0 () =
  Alcotest.(check int)
    "same"
    1
    (Fix16.to_int (Fix16.exp (Fix16.from_int 0)))

let exp1 () =
  Alcotest.(check int)
    "same"
    0
    (Fix16.to_int (Fix16.exp Fix16.min))

let exp2 () =
  Alcotest.(check int)
    "same"
    (Fix16.to_int Fix16.max)
    (Fix16.to_int (Fix16.exp Fix16.max))

let exp_small () =
  Alcotest.(check bool)
    "max_delta < 0.01 over -11..4 with full precision"
    true
    (let max_delta = ref (-1.) in
     let worst = ref 0. in
     for i_int = -11 to 4 do
       for i_dec = 0 to 999999 do
         let i = (float_of_int i_int) +. (float_of_int i_dec) /. 1000000. in
         let result = Fix16.exp (Fix16.from_float i) in
         let fresult  = exp i in
         let delta = Float.abs ((Fix16.to_float result) -. fresult) in
         if delta > !max_delta
         then ( max_delta := delta ;
                worst := i ;
              )
       done ;
     done ;
     Printf.printf "worst:%f delta:%f\n" !worst !max_delta ;
     !max_delta < 0.01)

let exp_large () =
 Alcotest.(check bool)
   "max_delta <= 1. over full range"
   true
   (let max_delta = ref (-1.) in
    let worst = ref 0. in
    for i_int = 0 to 32767 do
      for i_dec = 0 to 9 do
        let i = (float_of_int i_int) +. (float_of_int i_dec) /. 10. in
        let result = Fix16.exp (Fix16.from_float i) in
        let fresult  = exp i in
        if fresult > 1000.
        then let delta = ((Float.abs ((Fix16.to_float result) -. fresult)) -. 1.) /. fresult in
        if delta > !max_delta
        then ( max_delta := delta ;
               worst := i ;
             )
      done ;
    done ;
    Printf.printf "worst:%f delta:%f\n" !worst !max_delta ;
    !max_delta <= 1.)

let ln () =
  Alcotest.(check bool)
    "max_delta <= 1.1 over full range"
    true
    (let max_delta = ref (-1.) in
     let worst = ref 0. in
     for i_int =  0 to 32767 do
       for i_dec = 0 to 9 do
         let i = (float_of_int i_int) +. (float_of_int i_dec) /. 10. in
         let result = Fix16.ln (Fix16.from_float i) in
         let fresult  = log i in
         let delta = (Float.abs ((Fix16.to_float result) -. fresult)) /. fresult in
         if delta > !max_delta
         then ( max_delta := delta ;
                worst := i ;
              )
       done ;
     done ;
     Printf.printf "worst:%f delta:%f\n" !worst !max_delta ;
     !max_delta <= 1.)


let buf = Bigstring.create 13

let str0 () =
  Fix16.to_bigstring (Fix16.from_float 1234.5678) buf 4 ;
  Alcotest.(check string)
    "same string"
    "1234.5678"
    (Bigstring.sub_string buf 0 9)

let str1 () =
  Fix16.to_bigstring (Fix16.from_float (-1234.5678)) buf 4 ;
  Alcotest.(check string)
    "same string"
    "-1234.5678"
    (Bigstring.sub_string buf 0 10)

let str2 () =
  Fix16.to_bigstring (Fix16.from_int 0) buf 0 ;
  Alcotest.(check string)
    "same string"
    "0"
    (Bigstring.sub_string buf 0 1)

let str3 () =
  Fix16.to_bigstring (Fix16.from_float 0.9) buf 0 ;
  Alcotest.(check string)
    "same string"
    "1"
    (Bigstring.sub_string buf 0 1)

let str4 () =
  Fix16.to_bigstring Fix16.max buf 5 ;
  Alcotest.(check string)
    "same string"
    "32767.99998"
    (Bigstring.sub_string buf 0 11)

let str5 () =
  Fix16.to_bigstring Fix16.min buf 5 ;
  Alcotest.(check string)
    "same string"
    "-32768.00000"
    (Bigstring.sub_string buf 0 12)


let tests = [
    "from and to int",                        `Quick, convert_int ;
    "from and to negative int",               `Quick, convert_int_neg ;
    "from and to int32",                      `Quick, convert_int32 ;

    "add precision tests",                    `Quick, add ;
    "sub precision tests",                    `Quick, sub ;

    "mul test 0",                             `Quick, mul0 ;
    "mul test 1",                             `Quick, mul1 ;
    "mul test 2",                             `Quick, mul2 ;
    "mul test 3",                             `Quick, mul3 ;
    "mul precision tests",                    `Quick, mul ;

    "div test 0",                             `Quick, div0 ;
    "div test 1",                             `Quick, div1 ;
    "div test 2",                             `Quick, div2 ;
    "div test 3",                             `Quick, div3 ;
    "div precision tests",                    `Quick, div ;

    "exp test 0",                             `Quick, exp0 ;
    "exp test 1",                             `Quick, exp1 ;
    "exp test 2",                             `Quick, exp2 ;
    "exp small number precision test",        `Slow, exp_small ;
    "exp large number precision test",        `Quick, exp_large ;
    "ln precision test",                      `Quick, ln ;

    "string test 0",                          `Quick, str0 ;
    "string test 1",                          `Quick, str1 ;
    "string test 2",                          `Quick, str2 ;
    "string test 3",                          `Quick, str3 ;
    "string test 4",                          `Quick, str4 ;
    "string test 5",                          `Quick, str5 ;
  ]

let () =
  Alcotest.run "libfixmath" [
      "tests", tests;
    ]
