// Copyright 2012 The Rust Project Developers. See the COPYRIGHT
// file at the top-level directory of this distribution and at
// http://rust-lang.org/COPYRIGHT.
//
// Licensed under the Apache License, Version 2.0 <LICENSE-APACHE or
// http://www.apache.org/licenses/LICENSE-2.0> or the MIT license
// <LICENSE-MIT or http://opensource.org/licenses/MIT>, at your
// option. This file may not be copied, modified, or distributed
// except according to those terms.

struct cat {
    meows : uint,

    how_hungry : int,
    name : String,
}

impl cat {
    pub fn speak(&mut self) { self.meow(); }

    pub fn eat(&mut self) -> bool {
        if self.how_hungry > 0 {
            println!("OM NOM NOM");
            self.how_hungry -= 2;
            return true;
        } else {
            println!("Not hungry!");
            return false;
        }
    }
}

impl cat {
    fn meow(&mut self) {
        println!("Meow");
        self.meows += 1u;
        if self.meows % 5u == 0u {
            self.how_hungry += 1;
        }
    }
}

fn cat(in_x : uint, in_y : int, in_name: String) -> cat {
    cat {
        meows: in_x,
        how_hungry: in_y,
        name: in_name
    }
}

pub fn main() {
  let mut nyan = cat(0u, 2, "nyan".to_string());
  nyan.eat();
  assert!((!nyan.eat()));
  for _ in 1u..10u { nyan.speak(); };
  assert!((nyan.eat()));
}
