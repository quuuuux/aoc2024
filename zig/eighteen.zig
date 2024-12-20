// I have no idea what I am doing
const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

const Error = error{
   WrongArguments,
   InvalidArgument,
   UnsolvableMaze,
};

fn parse(str: []u8) ![]struct { u8, u8 } {
   var nrows: usize = 0;
   var str1 = str;
   while (std.mem.indexOfScalar(u8, str1, '\n')) |i| {
      nrows += 1;
      str1 = str1[i + 1 ..];
   }

   const rows = try allocator.alloc(struct { u8, u8 }, nrows);
   str1 = str;
   var i: usize = 0;
   while (std.mem.indexOfScalar(u8, str1, ',')) |d| {
      const x = try std.fmt.parseInt(u8, str1[0..d], 10);
      str1 = str1[d + 1 ..];
      const d1 = std.mem.indexOfScalar(u8, str1, '\n').?;
      const y = try std.fmt.parseInt(u8, str1[0..d1], 10);
      str1 = str1[d1 + 1 ..];
      rows[i] = .{ x, y };
      i += 1;
   }
   return rows;
}

const Tile = enum(u2) {
   Empty,
   Blocked,
   Visited,
};

const Point = struct {
   x: i32,
   y: i32,
   cost: u32,
};

const Ring = struct {
   buf: [1024]Point,
   head: usize,
   tail: usize,
};

fn fetch_add(x: anytype, y: anytype) @TypeOf(x.*, y) {
   const x1 = x.*;
   x.* += y;
   return x1;
}

fn tryStep(
   grid: *[71][71]Tile,
   ring: *Ring,
   x: isize,
   y: isize,
   cost: u32,
) void {
   if (x < 0 or 71 <= x or y < 0 or 71 <= y) {
      return;
   }
   if (grid[@intCast(y)][@intCast(x)] != Tile.Empty) {
      return;
   }
   grid[@intCast(y)][@intCast(x)] = Tile.Visited;
   ring.buf[fetch_add(&ring.head, 1) & 0x3ff] = .{
      .x = @intCast(x),
      .y = @intCast(y),
      .cost = cost,
   };
}

fn one(input: []const struct { u8, u8 }, nblocks: usize) !u32 {
   var grid = std.mem.zeroes([71][71]Tile);
   for (input[0..nblocks]) |r| {
      const x: u8, const y: u8 = r;
      grid[y][x] = Tile.Blocked;
   }

   var ring = Ring{ .buf = undefined, .head = 0, .tail = 0 };
   ring.buf[fetch_add(&ring.head, 1)] = .{ .x = 0, .y = 0, .cost = 0 };
   while (ring.head != ring.tail) {
      const p = ring.buf[fetch_add(&ring.tail, 1) & 0x3ff];
      if (p.x == 70 and p.y == 70) {
         return p.cost;
      }
      tryStep(&grid, &ring, p.x, p.y - 1, p.cost + 1);
      tryStep(&grid, &ring, p.x, p.y + 1, p.cost + 1);
      tryStep(&grid, &ring, p.x - 1, p.y, p.cost + 1);
      tryStep(&grid, &ring, p.x + 1, p.y, p.cost + 1);
   }
   return Error.UnsolvableMaze;
}

fn two(input: []const struct { u8, u8 }) struct { u8, u8 } {
   var i: usize = 1;
   var j: usize = input.len;
   while (i < j) {
      const k = (i + j) / 2;
      if (one(input, k)) |_| {
         i = k + 1;
      } else |_| {
         j = k - 1;
      }
   }
   return input[i - 1];
}

pub fn main() !void {
   if (std.os.argv.len != 2) {
      return Error.WrongArguments;
   }
   const str = try std
      .fs
      .cwd()
      .readFileAlloc(allocator, std.mem.span(std.os.argv[1]), 0x8000);
   const input = try parse(str);
   defer allocator.free(input);
   std.debug.print("{}\n", .{try one(input, 1024)});
   std.debug.print("{}\n", .{two(input)});
}
