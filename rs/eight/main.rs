use std::collections::{HashMap, HashSet};
use std::error::Error;
use std::{env, fs};

use aoc2024::box_error;

fn parse_input(s: &str) -> Result<(Vec<Vec<u8>>, HashMap<u8, Vec<(i32, i32)>>), Box<dyn Error>> {
   s.split_terminator('\n').enumerate().try_fold(
      (vec![], HashMap::new()),
      |(mut rows, mut ants), (y, ln)| {
         let row = ln
            .as_bytes()
            .iter()
            .copied()
            .enumerate()
            .inspect(|&(x, c)| {
               if c != '.' as u8 {
                  ants
                     .entry(c)
                     .and_modify(|ps: &mut Vec<_>| ps.push((x as i32, y as i32)))
                     .or_insert_with(|| vec![(x as i32, y as i32)]);
               }
            })
            .map(|(_, c)| c)
            .collect::<Vec<_>>();
         if rows.get(0).map(|r: &Vec<u8>| r.len()).unwrap_or(row.len()) == row.len() {
            rows.push(row);
            Ok((rows, ants))
         } else {
            Err(box_error("invalid input"))
         }
      },
   )
}

fn insert_node(
   ps: &mut HashSet<(i32, i32)>,
   (x_max, y_max): (i32, i32),
   (x, y): (i32, i32),
) -> bool {
   if x < 0 || x >= x_max || y < 0 || y >= y_max {
      return false;
   }
   ps.insert((x, y));
   true
}

fn find_nodes_one(max: (i32, i32), ants: &HashMap<u8, Vec<(i32, i32)>>) -> HashSet<(i32, i32)> {
   ants.iter().fold(HashSet::new(), |mut acc, (_, ps)| {
      let mut ps = &ps[..];
      while ps.len() > 1 {
         let (x, y) = ps[0];
         ps = &ps[1..];
         for &(x1, y1) in ps {
            let dx = x - x1;
            let dy = y - y1;
            insert_node(&mut acc, max, (x + dx, y + dy));
            insert_node(&mut acc, max, (x1 - dx, y1 - dy));
         }
      }
      acc
   })
}

fn find_nodes_two(max: (i32, i32), ants: &HashMap<u8, Vec<(i32, i32)>>) -> HashSet<(i32, i32)> {
   ants.iter().fold(HashSet::new(), |mut acc, (_, ps)| {
      let mut ps = &ps[..];
      while ps.len() > 1 {
         let (x, y) = ps[0];
         acc.insert((x, y));
         ps = &ps[1..];
         for &(x1, y1) in ps {
            acc.insert((x1, y1));
            let dx = x - x1;
            let dy = y - y1;
            let (mut x2, mut y2) = (x + dx, y + dy);
            while insert_node(&mut acc, max, (x2, y2)) {
               (x2, y2) = (x2 + dx, y2 + dy);
            }
            let (mut x2, mut y2) = (x1 - dx, y1 - dy);
            while insert_node(&mut acc, max, (x2, y2)) {
               (x2, y2) = (x2 - dx, y2 - dy);
            }
         }
      }
      acc
   })
}

fn main() -> Result<(), Box<dyn Error>> {
   let arg = env::args().nth(1).ok_or_else(|| box_error("missing argument"))?;
   let (rows, ants) = parse_input(&fs::read_to_string(arg)?)?;
   if rows.len() == 0 {
      return Err(box_error("invalid input"));
   }
   let nodes = find_nodes_one((rows[0].len() as i32, rows.len() as i32), &ants);
   println!("{}", nodes.len());
   let nodes = find_nodes_two((rows[0].len() as i32, rows.len() as i32), &ants);
   println!("{}", nodes.len());
   Ok(())
}
