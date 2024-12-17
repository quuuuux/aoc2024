#if 0
cc -Wall -Wextra -g -fsanitize=address $0
./a.out $@
exit
#endif
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

void *
xmalloc(size_t len) {
   auto p = malloc(len);
   if (!p) {
      abort();
   }
   return p;
}

void *
xrealloc(void * p, size_t len) {
   p = realloc(p, len);
   if (!p) {
      abort();
   }
   return p;
}

Pair(const char *, size_t)
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
   Pair(int64_t, int64_t) p, v;
} Robo;

// Screw error handling
Pair(int64_t, const char *)
parse_int(const char * str, const char * end) {
   int64_t n = 0;
   int64_t s = str != end && *str == '-' ? ++str, -1 : 1;
   while (str != end) {
      auto c = *str - '0';
      if (c < 0 || c > 9) {
         break;
      }
      n *= 10;
      n += c;
      ++str;
   }
   return (__typeof(parse_int(str, end))){n * s, str};
}

Pair(Robo *, size_t)
parse(const char *str, size_t len) {
   size_t l = 0;
   for (size_t i = 0; i < len; ++i) {
      if (str[i] == '\n') {
         ++l;
      }
   }

   Robo *buf = xmalloc(l * sizeof(*buf));
   auto end = str + len;
   for (size_t i = 0; i < l; ++i) {
      str += __builtin_strlen("p=");
      auto r = parse_int(str, end);
      buf[i].p._0 = r._0;
      str = r._1;
      str += __builtin_strlen(",");
      r = parse_int(str, end);
      buf[i].p._1 = r._0;
      str = r._1;

      str += __builtin_strlen(" v=");
      r = parse_int(str, end);
      buf[i].v._0 = r._0;
      str = r._1;
      str += __builtin_strlen(",");
      r = parse_int(str, end);
      buf[i].v._1 = r._0;
      str = r._1;
      str += __builtin_strlen("\n");
   }
   return (__typeof(parse(str, len))){buf, l};
}

int64_t actual_mod(int64_t a, int64_t b) {
   auto m = a % b;
   if (m < 0) {
      m += b < 0 ? -b : b;
   }
   return m;
}

int64_t
one(Robo *robos, size_t nrobos) {
   int64_t quads[4] = {0};;
   for (size_t i = 0; i < nrobos; ++i) {
      auto x = actual_mod(robos[i].p._0 + (robos[i].v._0 * 100), 101);
      auto y = actual_mod(robos[i].p._1 + (robos[i].v._1 * 100), 103);
      if (x != 50 && y != 51) {
         ++quads[((x < 50) << 1) | (y < 51)];
      }
   }
   return quads[0] * quads[1] * quads[2] * quads[3];
}

int64_t
step(Robo *robos, size_t nrobos) {
   int64_t quads[4] = {0};;
   for (size_t i = 0; i < nrobos; ++i) {
      auto x = actual_mod((robos[i].p._0 += robos[i].v._0), 101);
      auto y = actual_mod((robos[i].p._1 += robos[i].v._1), 103);
      if (x != 50 && y != 51) {
         ++quads[((x < 50) << 1) | (y < 51)];
      }
   }
   return quads[0] * quads[1] * quads[2] * quads[3];
}

size_t
two(Robo *robos, size_t nrobos) {
   int64_t min = INT64_MAX;
   size_t j;
   for (size_t i = 0; i < 101 * 103; ++i) {
      auto s = step(robos, nrobos);
      if (s < min) {
         min = s;
         j = i;
      }
   }
   return j + 1;
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
   printf("%ld\n", one(games._0, games._1));
   printf("%ld\n", two(games._0, games._1));
   return 0;
fail:
   fprintf(stderr, "invalid argument\n");
   return -1;
}
