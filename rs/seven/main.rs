use std::error::Error;
use std::{env, fs};

use aoc2024::box_error;

fn parse_input(s: &str) -> Result<Vec<(u64, Vec<u64>)>, Box<dyn Error>> {
   s.split_terminator('\n').try_fold(vec![], |mut acc, ln| {
      let Some((goal, terms)) = ln.split_once(": ") else {
         return Err(box_error("invalid input"));
      };
      let goal = goal.parse()?;
      let terms = terms.split(' ').map(str::parse).collect::<Result<Vec<_>, _>>()?;
      if terms.is_empty() || terms.len() > 64 {
         return Err(box_error("unsupported input"));
      }
      acc.push((goal, terms));
      Ok(acc)
   })
}

fn is_solvable_one((goal, terms): &(u64, Vec<u64>)) -> bool {
   for i in 0..(1 << terms.len() - 1) {
      let n = terms[1..].iter().copied().enumerate().fold(terms[0], |acc, (j, x)| {
         if i & (1 << j) == 0 {
            acc + x
         } else {
            acc * x
         }
      });
      if n == *goal {
         return true;
      }
   }
   false
}

fn is_solvable_two(goal: u64, terms: &[u64]) -> bool {
   let x = terms[terms.len() - 1];
   match &terms[..terms.len() - 1] {
      [] => x == goal,
      xs => {
         if goal % x == 0 && is_solvable_two(goal / x, xs) {
            return true;
         }
         if goal >= x && is_solvable_two(goal - x, xs) {
            return true;
         }
         let d = 10u64.pow((x as f64).log10().floor() as u32 + 1);
         goal % d == x && is_solvable_two(goal / d, xs)
      }
   }
}

fn main() -> Result<(), Box<dyn Error>> {
   let arg = env::args().nth(1).ok_or_else(|| box_error("missing argument"))?;
   let rows = parse_input(&fs::read_to_string(arg)?)?;
   println!(
      "{}",
      rows.iter().fold(0, |acc, row| {
         if is_solvable_one(row) {
            acc + row.0
         } else {
            acc
         }
      })
   );
   println!(
      "{}",
      rows.iter().fold(0, |acc, (goal, terms)| {
         if is_solvable_two(*goal, &terms) {
            acc + *goal
         } else {
            acc
         }
      })
   );
   Ok(())
}
