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
xrealloc(void *p, size_t len) {
   p = realloc(p, len);
   if (!p) {
      abort();
   }
   return p;
}

Pair(int8_t *, size_t)
slurp(const char *fname) {
   int8_t *buf = NULL;
   size_t cap = 0;
   auto fd = try (open(fname, O_RDONLY));
   cap = try (lseek(fd, 0, SEEK_END)) + 1;
   try(lseek(fd, 0, SEEK_SET));
   buf = xmalloc(cap);

   for (size_t len = 0;;) {
      size_t len1 = try (read(fd, buf, cap - len));
      if (len1 == 0) {
         if (buf[len - 1] == '\n') {
            --len;
         }
         for (size_t i = 0; i < len; ++i) {
            if (buf[i] < '0' || '9' < buf[i]) {
               goto fail;
            }
            buf[i] = buf[i] - '0';
         }
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

size_t
one(uint8_t *buf, size_t len) {
   size_t sum = 0, i = 0, j = (len - 1) & ~0x1, k = 0;
   for (; i < j; ++i) {
      if (buf[i] == 0) {
         continue;
      }
      if (i % 2 == 0) {
         for (size_t l = 0; l < buf[i]; ++l) {
            sum += (i / 2) * k++;
         }
         continue;
      }

      for (size_t l = 0; l < buf[i]; ++l) {
         while (buf[j]-- == 0) {
            if ((j -= 2) <= i) {
               return sum;
            }
         }
         sum += (j / 2) * k++;
      }
   }

   if ((i = ((i + 1) & ~0x1)) < len && buf[i] != 0) {
      for (size_t l = 0; l < buf[i]; ++l) {
         sum += (i / 2) * k++;
      }
   }
   return sum;
}

// And this is when I start to regret choosing C
typedef struct {
   uint32_t len, pos, idx;
} Hole;

typedef struct {
   Hole *tail, *head, *begin, *end;
} HoleDeque;

void
hole_realloc(HoleDeque *vec) {
   auto cap = vec->end - vec->begin;
   auto off = (vec->end - vec->begin) / 2;
   vec->begin = xrealloc(vec->begin, cap * 2 * sizeof(Hole));
   vec->end = vec->begin + cap;
   vec->head = vec->begin + (vec->head - vec->tail) + off;
   vec->tail = vec->begin + off;
}

void
hole_push(HoleDeque *vec, Hole hole) {
   if (__builtin_expect(vec->head == vec->end, 0)) {
      hole_realloc(vec);
   }
   *vec->head++ = hole;
}

void
hole_insert(HoleDeque *vec, Hole hole) {
   ssize_t i = 0, j = vec->head - vec->tail - 1;
   while (i < j) {
      auto k = (i + j) / 2;
      if (vec->tail[k].pos <= hole.pos) {
         i = k + 1;
      } else {
         j = k - 1;
      }
   }
   if (hole.pos > vec->tail[i].pos) {
      ++i;
   }

   if (i < (vec->head - vec->tail) / 2) {
      if (__builtin_expect(vec->tail == vec->begin, 0)) {
         hole_realloc(vec);
      }
      __builtin_memmove(vec->tail - 1, vec->tail, i * sizeof(Hole));
      --vec->tail;
   } else {
      if (__builtin_expect(vec->head == vec->end, 0)) {
         hole_realloc(vec);
      }
      __builtin_memmove(
         vec->tail + i + 1,
         vec->tail + i,
         (vec->head++ - vec->tail - i) * sizeof(Hole));
   }
   vec->tail[i] = hole;
}

ssize_t
hole_fill(HoleDeque holes[static 9], size_t len, size_t idx) {
   size_t l = len - 1;
   auto pos = holes[l].head != holes[l].tail ?
      holes[l].tail[0].pos :
      UINT32_MAX;
   for (size_t i = l + 1; i < 9; ++i) {
      if (holes[i].head != holes[i].tail && holes[i].tail[0].pos < pos) {
         pos = holes[(l = i)].tail[0].pos;
      }
   }
   if (pos == UINT32_MAX || holes[l].tail[0].idx > idx) {
      return -1;
   }

   auto hole = *holes[l].tail++;
   if ((hole.len -= len) > 0) {
      hole.pos += len;
      hole_insert(holes + hole.len - 1, hole);
   }
   return pos;
}

size_t
two(int8_t *buf, size_t len) {
   HoleDeque holes[9];
   for (size_t i = 0; i < 9; ++i) {
      Hole *p = xmalloc(4096 * sizeof(*p));
      holes[i] = (HoleDeque){p + 1024, p + 1024, p, p + 4096};
   }
   for (size_t i = 1, k = 0; i < len; i += 2) {
      k += buf[i - 1];
      if (buf[i] != 0) {
         hole_push(holes + buf[i] - 1, (Hole){buf[i], k, i});
      }
      k += buf[i];
   }

   size_t sum = 0;
   for (size_t j = (len - 1) & ~0x1; j > 0; j -= 2) {
      auto pos = hole_fill(holes, buf[j], j);
      if (pos < 0) {
         continue;
      }
      for (size_t l = pos; l < (size_t)pos + buf[j]; ++l) {
         sum += (j / 2) * l;
      }
      buf[j] *= -1;
   }
   for (size_t i = 0, k = 0; i < len; i += 2) {
      if (buf[i] <= 0) {
         k += -buf[i];
      } else {
         for (size_t l = 0; l < (uint8_t)buf[i]; ++l) {
            sum += (i / 2) * k++;
         }
      }
      if (i + 1 < len) {
         k += buf[i + 1];
      }
   }

   for (size_t i = 0; i < 9; ++i) {
      free(holes[i].begin);
   }
   return sum;
}

int
main(int argc, char **argv) {
   if (argc != 2) {
      fprintf(stderr, "invalid argument\n");
      return -1;
   }
   auto nums = slurp(argv[1]);
   if (!nums._0) {
      fprintf(stderr, "invalid argument\n");
      return -1;
   }

   uint8_t *dup = xmalloc(nums._1);
   __builtin_memcpy(dup, nums._0, nums._1);
   printf("%zu\n", one(dup, nums._1));
   printf("%zu\n", two(nums._0, nums._1));

   free(dup);
   free(nums._0);
   return 0;
}
