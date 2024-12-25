// I still have no idea what I am doing
const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

const Error = error{
   WrongArguments,
   InvalidArgument,
};

const Input = struct {
   patterns: std.ArrayList([]const u8),
   targets: std.ArrayList([]const u8),

   fn deinit(self: Input) void {
      self.patterns.deinit();
      self.targets.deinit();
   }
};

fn parse(str: []u8) !Input {
   const idx = if (std.mem.indexOfScalar(u8, str, '\n')) |i| i else {
      return Error.InvalidArgument;
   };
   const s = str[0..idx];
   var pats = std.ArrayList([]const u8).init(allocator);
   errdefer parts.deinit();
   var toks = std.mem.tokenizeSequence(u8, s, ", ");
   while (toks.next()) |tok| {
      if (tok.len == 0) {
         return Error.InvalidArgument;
      }
      try pats.append(tok);
   }

   if (str.len < idx + 3) {
      return Error.InvalidArgument;
   }
   const s1 = if (std.mem.indexOfScalar(u8, str[idx + 2 ..], '\n')) |j|
      str[j..]
   else {
      return Error.InvalidArgument;
   };
   var targs = std.ArrayList([]const u8).init(allocator);
   errdefer targs.deinit();
   var lns = std.mem.tokenizeScalar(u8, s1, '\n');
   while (lns.next()) |ln| {
      if (ln.len == 0) {
         return Error.InvalidArgument;
      }
      try targs.append(ln);
   }
   return Input{ .patterns = pats, .targets = targs };
}

pub fn u8SliceAsc(_: void, a: []const u8, b: []const u8) bool {
   return std.mem.lessThan(u8, a, b);
}

pub fn u8OrderFirst(a: u8, b: []const u8) std.math.Order {
   return std.math.order(a, b[0]);
}

pub fn one(input: Input) !u32 {
   var sum: u32 = 0;
   var stk = std.ArrayList(usize).init(allocator);
   defer stk.deinit();
   var seen = std.AutoHashMap(usize, void).init(allocator);
   defer seen.deinit();
   const pats = input.patterns.items;

   for (input.targets.items) |targ| {
      try stk.append(0);
      outer: while (stk.popOrNull()) |i| {
         const t = targ[i..];
         // Didn't feel like writing a radix tree...
         var j = std.sort.lowerBound([]const u8, pats, t[0], u8OrderFirst);
         while (j < pats.len and pats[j][0] == t[0]) : (j += 1) {
            if (std.mem.startsWith(u8, t, pats[j])) {
               const k = i + pats[j].len;
               if (k == targ.len) {
                  sum += 1;
                  break :outer;
               }

               const r = try seen.getOrPut(k);
               if (!r.found_existing) {
                  try stk.append(k);
               }
            }
         }
      }
      stk.clearRetainingCapacity();
      seen.clearRetainingCapacity();
   }
   return sum;
}

pub fn count(memo: *std.StringHashMap(u64), pats: [][]const u8, targ: []const u8) !u64 {
   if (memo.get(targ)) |n| {
      return n;
   }

   var sum: u64 = 0;
   var i = std.sort.lowerBound([]const u8, pats, targ[0], u8OrderFirst);
   while (i < pats.len and pats[i][0] == targ[0]) : (i += 1) {
      if (std.mem.startsWith(u8, targ, pats[i])) {
         sum += if (pats[i].len == targ.len)
            1
         else
            try count(memo, pats, targ[pats[i].len..]);
      }
   }
   try memo.put(targ, sum);
   return sum;
}

pub fn two(input: Input) !u64 {
   var sum: u64 = 0;
   var memo = std.StringHashMap(u64).init(allocator);
   defer memo.deinit();
   const pats = input.patterns.items;
   for (input.targets.items) |targ| {
      sum += try count(&memo, pats, targ);
   }
   return sum;
}

pub fn main() !void {
   if (std.os.argv.len != 2) {
      return Error.WrongArguments;
   }
   const str = try std
      .fs
      .cwd()
      .readFileAlloc(allocator, std.mem.span(std.os.argv[1]), 0x8000);
   defer allocator.free(str);
   var input = try parse(str);
   defer input.deinit();
   std.mem.sort([]const u8, input.patterns.items, {}, u8SliceAsc);
   std.debug.print("{}\n", .{try one(&input)});
   std.debug.print("{}\n", .{try two(&input)});
}
