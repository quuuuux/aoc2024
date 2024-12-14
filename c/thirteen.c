#include <fcntl.h>
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

typedef struct {
   Pair(int64_t, int64_t) a, b, p;
} Game;

// Screw error handling
Pair(int64_t, const char*)
parse_int(const char* str, const char* end) {
   int64_t n = 0;
   while (str != end) {
      auto c = *str - '0';
      if (c < 0 || c > 9) {
         break;
      }
      n *= 10;
      n += c;
      ++str;
   }
   return (__typeof(parse_int(str, end))){n, str};
}

Pair(Game*, size_t)
parse(const char *str, size_t len) {
   size_t l = 0;
   for (size_t i = 0; i < len; ++i) {
      if (str[i] == '\n') {
         ++l;
      }
   }
   l = (l + 1) / 4;

   Game* buf = xmalloc(l * sizeof(*buf));
   auto end = str + len;
   for (size_t i = 0; i < l; ++i) {
      str += __builtin_strlen("Button A: X+");
      auto r = parse_int(str, end);
      buf[i].a._0 = r._0;
      str = r._1;
      str += __builtin_strlen(", Y+");
      r = parse_int(str, end);
      buf[i].a._1 = r._0;
      str = r._1;

      str += __builtin_strlen("\nButton B: X+");
      r = parse_int(str, end);
      buf[i].b._0 = r._0;
      str = r._1;
      str += __builtin_strlen(", Y+");
      r = parse_int(str, end);
      buf[i].b._1 = r._0;
      str = r._1;

      str += __builtin_strlen("\nPrize: X=");
      r = parse_int(str, end);
      buf[i].p._0 = r._0;
      str = r._1;
      str += __builtin_strlen(", Y=");
      r = parse_int(str, end);
      buf[i].p._1 = r._0;
      str = r._1;
      str += __builtin_strlen("\n\n");
   }
   return (__typeof(parse(str, len))){buf, l};
}

int64_t
run(Game* games, size_t ngames, int64_t extra) {
   int64_t sum = 0;
   for (size_t i = 0; i < ngames; ++i) {
      auto g = games + i;
      g->p._0 += extra;
      g->p._1 += extra;
      // (a.0 * x) + (b.0 * y) = p.0
      // (a.1 * x) + (b.1 * y) = p.1
      //
      // x = (p.0 - (b.0 * y)) / a.0
      //
      // (a.1 * (p.0 - (b.0 * y)) / a.0) + (b.1 * y) = p.1
      // (((a.1 * p.0) - (a.1 * b.0 * y)) / a.0) + (b.1 * y) = p.1
      // (a.1 * p.0) - (a.1 * b.0 * y) + (a.0 * b.1 * y) = a.0 * p.1
      //
      // (a.1 * p.0) - (a.0 * p.1) = (a.1 * b.0 * y) - (a.0 * b.1 * y)
      // (a.1 * p.0) - (a.0 * p.1) = y * ((a.1 * b.0) - (a.0 * b.1))
      // y = ((a.1 * p.0) - (a.0 * p.1)) / ((a.1 * b.0) - (a.0 * b.1))
      auto y =
         ((g->a._1 * g->p._0) - (g->a._0 * g->p._1)) /
         ((g->a._1 * g->b._0) - (g->a._0 * g->b._1));
      auto x = (g->p._0 - (g->b._0 * y)) / g->a._0;
      sum += (g->a._0 * x) + (g->b._0 * y) == g->p._0 &&
         (g->a._1 * x) + (g->b._1 * y) == g->p._1 ?
            (3 * x) + y :
            0;
   }
   return sum;
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
   auto games = parse(str._0, str._1);
   if (!games._0) {
      goto fail;
   }
   printf("%ld\n", run(games._0, games._1, 0));
   printf("%ld\n", run(games._0, games._1, 10000000000000));
   return 0;
fail:
   fprintf(stderr, "invalid argument\n");
   return -1;
}
