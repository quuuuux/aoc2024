use std::error::Error;
use std::ops::AddAssign;

pub fn box_error<T: Into<Box<dyn Error>>>(x: T) -> Box<dyn Error> {
   x.into()
}

pub fn fetch_add<N: AddAssign + Copy + Sized>(x: &mut N, y: N) -> N {
   let z = *x;
   *x += y;
   z
}

pub trait Blit: Iterator + Sized {
   fn blit(mut self, buf: &mut [Self::Item]) -> (&mut [Self::Item], Self) {
      let mut i = 0;
      while let Some(elt) = self.next() {
         if i >= buf.len() {
            return (&mut buf[..i], self);
         }
         buf[i] = elt;
         i += 1;
      }
      (buf, self)
   }
}

impl<T: Iterator + Sized> Blit for T {}
