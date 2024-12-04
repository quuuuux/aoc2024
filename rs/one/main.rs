use std::error::Error;
use std::mem::replace;
use std::{env, fs};

use aoc2024::box_error;

fn parse_input(s: &str) -> Result<(Vec<u32>, Vec<u32>), Box<dyn Error>> {
   s.split_terminator('\n').try_fold((vec![], vec![]), |(mut xs, mut ys), ln| {
      let mut cols = ln.split_ascii_whitespace();
      match (cols.next(), cols.next(), cols.next()) {
         (Some(x), Some(y), None) => {
            xs.push(x.parse()?);
            ys.push(y.parse()?);
            Ok((xs, ys))
         },
         _ => Err(box_error("invalid input")),
      }
   })
}

fn advance(i: &mut usize, xs: &[u32]) -> Option<(u32, u32)> {
   let &x = xs.get(*i)?;
   let mut j = *i + 1;
   while xs.get(j).map_or(false, |&y| y == x) {
      j += 1;
   }
   Some((x, (j - replace(i, j)) as u32))
}

fn advance_to(i: &mut usize, x: u32, xs: &[u32]) -> Option<u32> {
   while x > *xs.get(*i)? {
      *i += 1;
   }
   if x < *xs.get(*i)? {
      return None;
   }

   let mut j = *i + 1;
   while xs.get(j).map_or(false, |&y| y == x) {
      j += 1;
   }
   Some((j - replace(i, j)) as u32)
}

fn main() -> Result<(), Box<dyn Error>> {
   let arg = env::args().nth(1).ok_or_else(|| box_error("missing argument"))?;
   let (mut xs, mut ys) = parse_input(&fs::read_to_string(arg)?)?;
   xs.sort_unstable();
   ys.sort_unstable();
   println!("{}", xs.iter().zip(&ys).fold(0, |acc, (&x, &y)| acc + x.abs_diff(y)));

   let mut score = 0;
   let (mut i, mut j) = (0, 0);
   while let Some((x, n)) = advance(&mut i, &xs) {
      score += x * n * advance_to(&mut j, x, &ys).unwrap_or(0);
   }
   println!("{score}");
   Ok(())
}
