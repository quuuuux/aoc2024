#if 0
cc -Wall -Wextra -g $0
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
   uint64_t a, b, c;
   uint8_t *code;
   size_t code_len;
} Vm;

// Screw error handling
Pair(uint64_t, const char *)
parse_int(const char *str, const char *end) {
   uint64_t n = 0;
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

Vm
parse(const char *str, size_t len) {
   auto end = str + len;
   str += __builtin_strlen("Register A: ");
   auto ra = parse_int(str, end);
   str = ra._1;
   str += __builtin_strlen("\nRegister B: ");
   auto rb = parse_int(str, end);
   str = rb._1;
   str += __builtin_strlen("\nRegister C: ");
   auto rc = parse_int(str, end);
   str = rc._1;
   str += __builtin_strlen("\n\nProgram: ");

   uint8_t *code = malloc((end - str) / 2);
   size_t i = 0;
   do {
      code[i++] = *str++ - '0';
   } while (++str != end);
   return (Vm){ra._0, rb._0, rc._0, code, i};
}

uint64_t
combo(Vm *m, uint8_t x) {
   switch (x) {
   case 0:
   case 1:
   case 2:
   case 3: return x;
   case 4: return m->a;
   case 5: return m->b;
   case 6: return m->c;
   default: __builtin_unreachable();
   }
}

size_t
one(Vm *m, uint8_t *out) {
   size_t i = 0;
   auto ip = m->code;
   while (ip != m->code + m->code_len) {
      switch (*ip++) {
      case 0:
         m->a >>= combo(m, *ip++);
         break;
      case 1:
         m->b ^= *ip++;
         break;
      case 2:
         m->b = combo(m, *ip++) & 0x7;
         break;
      case 3: {
         auto j = *ip++;
         if (m->a != 0) {
            ip = m->code + j;
         }
         break; }
      case 4:
         ip++;
         m->b ^= m->c;
         break;
      case 5:
         out[i++] = combo(m, *ip++) & 0x7;
         if (i == m->code_len) {
            return i;
         }
         break;
      case 6:
         m->b = m->a >> combo(m, *ip++);
         break;
      case 7:
         m->c = m->a >> combo(m, *ip++);
         break;
      default: __builtin_unreachable();
      }
   }
   return i;
}

// :)
const uint8_t GOAL[] = {2, 4, 1, 5, 7, 5, 1, 6, 0, 3, 4, 0, 5, 5, 3, 0};

// b <- a & 0x7
// b <- b ^ 5
// c <- a >> b
// b <- b ^ 6
// a <- a >> 3
// b <- b ^ c
// puts (b & 0x7)
// jnz 0
//
// c <- a >> ((a & 0x7) ^ 5)
// b <- ((a % 8) ^ 5) ^ 6
// puts ((b ^ c) & 0x7)
// a <- a >> 3
size_t
two(uint64_t a) {
   for (size_t i = 0; ; a >>= 3) {
      auto b = (a & 0x7) ^ 3;
      auto c = a >> ((a & 0x7) ^ 5);
      if (((b ^ c) & 0x7) != GOAL[i] || ++i == sizeof(GOAL)) {
         return i;
      }
   }
}

const uint8_t *
two_point_one(Vm *m, const uint8_t *goal) {
   auto ip = m->code;
   while (ip != m->code + m->code_len) {
      switch (*ip++) {
      case 0:
         m->a >>= combo(m, *ip++);
         break;
      case 1:
         m->b ^= *ip++;
         break;
      case 2:
         m->b = combo(m, *ip++) & 0x7;
         break;
      case 3: {
         auto j = *ip++;
         if (m->a != 0) {
            ip = m->code + j;
         }
         break; }
      case 4:
         ip++;
         m->b ^= m->c;
         break;
      case 5:
         if (*goal != (combo(m, *ip++) & 0x7)) {
            return goal;
         }
         ++goal;
         break;
      case 6:
         m->b = m->a >> combo(m, *ip++);
         break;
      case 7:
         m->c = m->a >> combo(m, *ip++);
         break;
      default: __builtin_unreachable();
      }
   }
   return goal;
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

   auto vm = parse(str._0, str._1);
   uint8_t *out = malloc(vm.code_len);
   auto len = one(&vm, out);
   printf("%u", out[0]);
   for (size_t i = 1; i < len; ++i) {
      printf(",%u", out[i]);
   }
   printf("\n");

   // Would take about a day
#if 0
   uint64_t i = 1ull << 45;
   for (; i < 1ull << 48 && two(i) != 16; ++i);
   printf("%lu\n", i);
#endif

   Pair(const uint8_t *, uint64_t) stk[3 * 16], *head = stk;
   head->_0 = vm.code + vm.code_len - 1;
   head++->_1 = 0;
   while (head-- != stk) {
      auto goal = head->_0;
      auto a = head->_1;
      for (size_t i = a; i < a + 8; ++i) {
         vm.a = i;
         vm.b = vm.c = 0;
         if (two_point_one(&vm, goal) == vm.code + vm.code_len) {
            if (goal == vm.code) {
               printf("%lu\n", i);
               goto done;
            }
            head->_0 = goal - 1;
            head++->_1 = i * 8;
         }
      }
   }
done:
   free(out);
   free(vm.code);
   free((void *)str._0);
   return 0;
fail:
   fprintf(stderr, "invalid argument\n");
   return -1;
}
