// Copyright 2013 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.
//

#![deny(overflowing_literals)]

fn test(x: i8) {
    println!("x {}", x);
}

#[allow(unused_variables)]
fn main() {
    let x1: u8 = 255; // should be OK
    let x1: u8 = 256; //~ error: literal out of range for u8

    let x1 = 255_u8; // should be OK
    let x1 = 256_u8; //~ error: literal out of range for u8

    let x2: i8 = -128; // should be OK
    let x1: i8 = 128; //~ error: literal out of range for i8
    let x2: i8 = --128; //~ error: literal out of range for i8

    let x3: i8 = -129; //~ error: literal out of range for i8
    let x3: i8 = -(129); //~ error: literal out of range for i8
    let x3: i8 = -{129}; //~ error: literal out of range for i8

    test(1000); //~ error: literal out of range for i8

    let x = 128_i8; //~ error: literal out of range for i8
    let x = 127_i8;
    let x = -128_i8;
    let x = -(128_i8);
    let x = -129_i8; //~ error: literal out of range for i8

    let x: i32 = 2147483647; // should be OK
    let x = 2147483647_i32; // should be OK
    let x: i32 = 2147483648; //~ error: literal out of range for i32
    let x = 2147483648_i32; //~ error: literal out of range for i32
    let x: i32 = -2147483648; // should be OK
    let x = -2147483648_i32; // should be OK
    let x: i32 = -2147483649; //~ error: literal out of range for i32
    let x = -2147483649_i32; //~ error: literal out of range for i32
    let x = 2147483648; //~ error: literal out of range for i32

    let x = 9223372036854775808_i64; //~ error: literal out of range for i64
    let x = -9223372036854775808_i64; // should be OK
    let x = 18446744073709551615_i64; //~ error: literal out of range for i64

    let x = -3.40282348e+38_f32; //~ error: literal out of range for f32
    let x =  3.40282348e+38_f32; //~ error: literal out of range for f32
    let x = -1.7976931348623159e+308_f64; //~ error: literal out of range for f64
    let x =  1.7976931348623159e+308_f64; //~ error: literal out of range for f64
}
