#if 0
cc -Wall -Wextra -g -fsanitize=address $0
./a.out $@
exit
#endif
#include <fcntl.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>

#define auto __auto_type

#define try(...) \
   __extension__ ({ \
      auto abcdefg = __VA_ARGS__; \
      if (abcdefg < 0) { \
         goto fail; \
      } \
      abcdefg; \
   })

#define try_bool(...) \
   __extension__ ({ \
      auto abcdefg = __VA_ARGS__; \
      if (!abcdefg) { \
         goto fail; \
      } \
      abcdefg; \
   })

#define Pair(T, U) struct { T _0; U _1; }

void*
xmalloc(size_t len) {
   auto p = malloc(len);
   if (!p) {
     abort();
   }
   return p;
}

void*
xrealloc(void* p, size_t len) {
   p = realloc(p, len);
   if (!p) {
     abort();
   }
   return p;
}

Pair(const char*, size_t)
slurp(const char *fname) {
   char *buf = NULL;
   size_t cap = 0;
   auto fd = try (open(fname, O_RDONLY));
   cap = try (lseek(fd, 0, SEEK_END)) + 1;
   try(lseek(fd, 0, SEEK_SET));
   buf = xmalloc(cap);

   for (size_t len = 0;;) {
     size_t len1 = try (read(fd, buf, cap - len));
     if (len1 == 0) {
       return (__typeof(slurp(fname))){buf, len};
     }

     len += len1;
     if (len == cap) {
       try_bool (!__builtin_umull_overflow(cap, 3, &cap));
       cap /= 2;
       buf = xrealloc(buf, cap);
     }
   }
fail:
   free(buf);
   return (__typeof(slurp(fname))){NULL, 0};
}

#define VISITED 0x1

typedef struct {
   char plant;
   uint8_t flags;
} Plot;

typedef struct {
   Plot *buf;
   size_t nrows, ncols;
} PlotVec2d;

PlotVec2d
parse(const char *str, size_t len) {
   const char* p = try_bool (__builtin_memchr(str, '\n', len));
   size_t nrows = 1, ncols = p - str;
   for (auto q = p + ncols + 1; q < str + len; q += ncols + 1) {
      try_bool (*q == '\n');
      ++nrows;
   }

   Plot* buf = xmalloc(ncols * nrows * sizeof(Plot)), *b = buf;
   for (auto q = str; q < str + len; q += ncols + 1) {
      for (size_t i = 0; i < ncols; ++i) {
         *b++ = (Plot){q[i], false};
      }
   }
   return (PlotVec2d){buf, nrows, ncols};
fail:
   return (PlotVec2d){NULL, 0, 0};
}

Plot*
plot_index(PlotVec2d *vec, size_t x, size_t y) {
   return vec->buf + (y * vec->ncols) + x;
}

Plot*
plot_try_index(PlotVec2d *vec, ssize_t x, ssize_t y) {
   return
      x >= 0 && (size_t)x < vec->ncols && y >= 0 && (size_t)y < vec->nrows ?
         vec->buf + (y * vec->ncols) + x :
         NULL;
}

bool
crawl_one_(char plant, Plot* plot, size_t *nfences) {
   if (!plot || plot->plant != plant) {
      ++*nfences;
      return false;
   }
   if ((plot->flags & VISITED) != 0) {
      return false;
   }
   plot->flags |= VISITED;
   return true;
}

size_t
crawl_one(PlotVec2d* plots, size_t x, size_t y) {
   Pair(uint32_t, uint32_t) stk[1024], *head = stk;
   *head++ = (__typeof(*head)){x, y};
   size_t nplots = 0, nfences = 0;
   while (head != stk) {
      ++nplots;
      auto h = *--head;
      auto c = plot_index(plots, h._0, h._1)->plant;
      if (crawl_one_(c, plot_try_index(plots, h._0, h._1 - 1), &nfences)) {
         *head++ = (__typeof(*head)){h._0, h._1 - 1};
      }
      if (crawl_one_(c, plot_try_index(plots, h._0, h._1 + 1), &nfences)) {
         *head++ = (__typeof(*head)){h._0, h._1 + 1};
      }
      if (crawl_one_(c, plot_try_index(plots, h._0 - 1, h._1), &nfences)) {
         *head++ = (__typeof(*head)){h._0 - 1, h._1};
      }
      if (crawl_one_(c, plot_try_index(plots, h._0 + 1, h._1), &nfences)) {
         *head++ = (__typeof(*head)){h._0 + 1, h._1};
      }
   }
   return nplots * nfences;
}

size_t
crawl(PlotVec2d *plots, size_t (*fn)(PlotVec2d*, size_t, size_t)) {
   size_t sum = 0;
   for (size_t i = 0; i < plots->ncols; ++i) {
      for (size_t j = 0; j < plots->nrows; ++j) {
         auto plot = plot_index(plots, i, j);
         if (plot->flags == 0) {
            plot->flags = VISITED;
            sum += fn(plots, i, j);
         }
      }
   }
   return sum;
}

bool
crawl_two_(char plant, Plot* plot, unsigned off, unsigned *walls) {
   if (!plot || plot->plant != plant) {
      *walls |= 1 << off;
      return false;
   }
   if ((plot->flags & VISITED) != 0) {
      return false;
   }
   plot->flags |= VISITED;
   return true;
}

bool
check_diagonal(PlotVec2d* plots, char plant, size_t x, size_t y) {
   auto p = plot_try_index(plots, x, y);
   return !p || p->plant != plant;
}

size_t
check_diagonals(
   PlotVec2d* plots, char plant, size_t x, size_t y, unsigned walls)
{
   size_t ncorners = 0;
   if ((walls & 0x3) == 0) { // Up left
      ncorners += __builtin_popcount(walls) == 2;
      ncorners += check_diagonal(plots, plant, x - 1, y - 1);
   }
   if ((walls & 0x6) == 0) { // Left down
      ncorners += __builtin_popcount(walls) == 2;
      ncorners += check_diagonal(plots, plant, x - 1, y + 1);
   }
   if ((walls & 0xc) == 0) { // Down right
      ncorners += __builtin_popcount(walls) == 2;
      ncorners += check_diagonal(plots, plant, x + 1, y + 1);
   }
   if ((walls & 0x9) == 0) { // Up right
      ncorners += __builtin_popcount(walls) == 2;
      ncorners += check_diagonal(plots, plant, x + 1, y - 1);
   }
   return ncorners;
}

size_t
crawl_two(PlotVec2d* plots, size_t x, size_t y) {
   Pair(uint32_t, uint32_t) stk[1024], *head = stk;
   *head++ = (__typeof(*head)){x, y};
   size_t nplots = 0, nfences = 0;
   while (head != stk) {
      ++nplots;
      auto h = *--head;
      auto c = plot_index(plots, h._0, h._1)->plant;
      unsigned walls = 0;
      if (crawl_two_(c, plot_try_index(plots, h._0, h._1 - 1), 0, &walls)) {
         *head++ = (__typeof(*head)){h._0, h._1 - 1};
      }
      if (crawl_two_(c, plot_try_index(plots, h._0, h._1 + 1), 2, &walls)) {
         *head++ = (__typeof(*head)){h._0, h._1 + 1};
      }
      if (crawl_two_(c, plot_try_index(plots, h._0 - 1, h._1), 1, &walls)) {
         *head++ = (__typeof(*head)){h._0 - 1, h._1};
      }
      if (crawl_two_(c, plot_try_index(plots, h._0 + 1, h._1), 3, &walls)) {
         *head++ = (__typeof(*head)){h._0 + 1, h._1};
      }

      switch (__builtin_popcount(walls)) {
      case 0:
      case 1:
      case 2: nfences += check_diagonals(plots, c, h._0, h._1, walls); break;
      case 3: nfences += 2; break;
      case 4: nfences += 4; break;
      default: __builtin_unreachable();
      }
   }
   return nplots * nfences;
}

int
main(int argc, char **argv) {
   if (argc != 2) {
      goto fail;
   }
   auto str = slurp(argv[1]);
   if (!str._0) {
      goto fail;
   }
   auto plots = parse(str._0, str._1);
   if (!plots.buf) {
      goto fail;
   }

   printf("%zu\n", crawl(&plots, crawl_one));
   for (size_t i = 0; i < plots.nrows * plots.ncols; ++i) {
      plots.buf[i].flags = 0;
   }
   printf("%zu\n", crawl(&plots, crawl_two));
   free(plots.buf);
   return 0;
fail:
   fprintf(stderr, "invalid argument\n");
   return -1;
}
