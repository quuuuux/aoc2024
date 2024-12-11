use std::collections::HashMap;
use std::error::Error;
use std::{env, fs};

use aoc2024::box_error;

fn parse_input(s: &str) -> Result<Vec<u64>, Box<dyn Error>> {
   s.split_ascii_whitespace().map(|s| s.parse().map_err(Into::into)).collect()
}

fn blink(memo: &mut HashMap<(u32, u64), u64>, i: u32, rock: u64) -> u64 {
   if i == 0 {
      return 1;
   }
   if rock == 0 {
      return if i == 1 { 1 } else { blink(memo, i - 2, 2024) };
   }
   let len = (rock as f64).log10().floor() as u32 + 1;
   if len % 2 != 0 {
      return blink(memo, i - 1, rock * 2024);
   }

   if let Some(&n) = memo.get(&(i, rock)) {
      return n;
   }
   let d = 10u64.pow(len / 2);
   let m = blink(memo, i - 1, rock / d) + blink(memo, i - 1, rock % d);
   memo.insert((i, rock), m);
   m
}

fn main() -> Result<(), Box<dyn Error>> {
   let arg = env::args().nth(1).ok_or_else(|| box_error("missing argument"))?;
   let rocks = parse_input(&fs::read_to_string(arg)?)?;
   let mut memo = HashMap::new();
   println!("{}", rocks.iter().copied().fold(0, |acc, r| acc + blink(&mut memo, 25, r)));
   println!("{}", rocks.iter().copied().fold(0, |acc, r| acc + blink(&mut memo, 75, r)));
   Ok(())
}
