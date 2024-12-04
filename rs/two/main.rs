use std::error::Error;
use std::num::ParseIntError;
use std::{env, fs};

use aoc2024::box_error;

fn parse_input(s: &str) -> Result<Vec<Vec<u32>>, ParseIntError> {
   s.split_terminator('\n')
      .map(|ln| ln.split_ascii_whitespace().map(|s| s.parse()).collect())
      .collect()
}

fn test_first(x: u32, y: u32) -> Option<bool> {
   match x.abs_diff(y) {
      1..=3 => Some(x < y),
      _ => None,
   }
}

fn test_rest(row: &[u32], lt: bool) -> bool {
   for (&x, &y) in row.iter().zip(&row[1..]) {
      if !matches!(x.abs_diff(y), 1..=3) || (x < y) != lt {
         return false;
      }
   }
   true
}

enum Answer {
   Good,
   Bad,
   Maybe,
}

fn test_rest_lenient(row: &[u32], lt: bool) -> Answer {
   for (i, (&x, &y)) in row.iter().zip(&row[1..]).enumerate() {
      if !matches!(x.abs_diff(y), 1..=3) || (x < y) != lt {
         if i + 2 >= row.len() {
            return Answer::Good;
         }

         return match test_first(x, row[i + 2]).map_or(false, |l| l == lt) {
            true => match test_rest(&row[i + 2..], lt) {
               true => Answer::Good,
               false if i == 0 => Answer::Maybe,
               false => Answer::Bad,
            },
            false if i == 0 => Answer::Maybe,
            false => Answer::Bad,
         };
      }
   }
   Answer::Good
}

fn main() -> Result<(), Box<dyn Error>> {
   let arg = env::args().nth(1).ok_or_else(|| box_error("missing argument"))?;
   let rows = parse_input(&fs::read_to_string(arg)?)?;

   println!(
      "{}",
      rows.iter().fold(0, |acc, row| {
         if row.len() < 2 {
            return acc + 1;
         }
         match test_first(row[0], row[1]) {
            Some(lt) => acc + test_rest(&row[1..], lt) as u32,
            None => acc,
         }
      }),
   );

   println!(
      "{}",
      rows.iter().fold(0, |acc, row| {
         if row.len() < 3 {
            return acc + 1;
         }

         match test_first(row[0], row[1]) {
            Some(lt) => match test_rest_lenient(&row[1..], lt) {
               Answer::Good => acc + 1,
               Answer::Bad => acc,
               Answer::Maybe => match test_first(row[1], row[2]) {
                  Some(lt1) if lt1 != lt => acc + test_rest(&row[2..], lt1) as u32,
                  _ => match test_first(row[0], row[2]) {
                     Some(lt) => acc + test_rest(&row[2..], lt) as u32,
                     None => acc,
                  },
               },
            },
            None => match test_first(row[0], row[2]) {
               Some(lt) => match test_rest(&row[2..], lt) {
                  true => acc + 1,
                  false => match test_first(row[1], row[2]) {
                     Some(lt1) if lt1 != lt => acc + test_rest(&row[2..], lt1) as u32,
                     _ => acc,
                  },
               },
               None => match test_first(row[1], row[2]) {
                  Some(lt) => acc + test_rest(&row[2..], lt) as u32,
                  None => acc,
               },
            },
         }
      })
   );
   Ok(())
}
