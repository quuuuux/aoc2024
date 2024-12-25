// I still have no idea what I am doing
const std = @import("std");

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
const allocator = gpa.allocator();

const Error = error{
    WrongArguments,
    InvalidArgument,
};

const Tile = packed struct {
    cost: i31,
    wall: bool,
};

const Point = struct {
    x: i32,
    y: i32,

    fn add(self: Point, dir: Point) Point {
        return Point{ .x = self.x + dir.x, .y = self.y + dir.y };
    }
};

const Input = struct {
    grid: [][]Tile,
    start: Point,
    end: Point,

    fn deinit(self: Input) void {
        for (self.grid) |row| {
            allocator.free(row);
        }
        allocator.free(self.grid);
    }

    fn tryIndex(self: Input, p: Point) ?Tile {
        if (p.x < 0 or self.grid[0].len <= p.x or
            p.y < 0 or self.grid.len <= p.y)
        {
            return null;
        }
        return self.grid[@intCast(p.y)][@intCast(p.x)];
    }
};

fn fetch_add(x: anytype, comptime y: anytype) @TypeOf(x.*, y) {
    const x1 = x.*;
    x.* += y;
    return x1;
}

fn parse(str: []u8) !Input {
    const rowlen = if (std.mem.indexOfScalar(u8, str, '\n')) |i| i else {
        return Error.InvalidArgument;
    };
    var nrows: usize = 1;
    var str1 = str[rowlen + 1 ..];
    while (std.mem.indexOfScalar(u8, str1, '\n')) |i| {
        if (i != rowlen) {
            return Error.InvalidArgument;
        }
        nrows += 1;
        str1 = str1[i + 1 ..];
    }

    const grid = try allocator.alloc([]Tile, nrows);
    var start: Point = undefined;
    var end: Point = undefined;
    var i: usize = 0;
    for (0..nrows) |y| {
        grid[y] = try allocator.alloc(Tile, rowlen);
        @memset(grid[y], Tile{ .cost = 0, .wall = false });
        for (0..rowlen) |x| {
            switch (str[fetch_add(&i, comptime 1)]) {
                '.' => {},
                '#' => grid[y][x].wall = true,
                'S' => start = Point{ .x = @intCast(x), .y = @intCast(y) },
                'E' => end = Point{ .x = @intCast(x), .y = @intCast(y) },
                else => return Error.InvalidArgument,
            }
        }
        i += 1;
    }
    return Input{ .grid = grid, .start = start, .end = end };
}

fn tryStep(input: Input, p: Point) bool {
    return if (input.tryIndex(p)) |t| !t.wall else false;
}

const directions = [_]Point{
    .{ .x = 0, .y = -1 },
    .{ .x = 0, .y = 1 },
    .{ .x = -1, .y = 0 },
    .{ .x = 1, .y = 0 },
};

fn solveInitial(input: Input) !std.ArrayList(Point) {
    var prev = input.start;
    var curr: Point = undefined;
    for (directions) |d| {
        const next = prev.add(d);
        if (tryStep(input, next)) {
            input.grid[@intCast(next.y)][@intCast(next.x)].cost = 1;
            curr = next;
            break;
        }
    } else {
        return Error.InvalidArgument;
    }

    var steps = std.ArrayList(Point).init(allocator);
    errdefer steps.deinit();
    try steps.append(prev);
    try steps.append(curr);
    while (!std.meta.eql(curr, input.end)) {
        for (directions) |d| {
            const next = curr.add(d);
            if (std.meta.eql(next, prev)) {
                continue;
            }
            if (tryStep(input, next)) {
                input.grid[@intCast(next.y)][@intCast(next.x)].cost =
                    input.grid[@intCast(curr.y)][@intCast(curr.x)].cost + 1;
                try steps.append(next);
                prev = curr;
                curr = next;
                break;
            }
        } else {
            return Error.InvalidArgument;
        }
    }
    return steps;
}

const cheatTwo = [_]Point{
    .{ .x = 0, .y = -2 },
    .{ .x = 0, .y = 2 },
    .{ .x = -2, .y = 0 },
    .{ .x = 2, .y = 0 },
    .{ .x = -1, .y = -1 },
    .{ .x = 1, .y = -1 },
    .{ .x = -1, .y = 1 },
    .{ .x = 1, .y = 1 },
};

fn one(input: Input, path: std.ArrayList(Point)) u32 {
    var sum: u32 = 0;
    for (path.items) |p| {
        const cost = input.grid[@intCast(p.y)][@intCast(p.x)].cost;
        for (cheatTwo) |d| {
            if (input.tryIndex(p.add(d))) |t| {
                if (t.cost - cost > 101) {
                    sum += 1;
                }
            }
        }
    }
    return sum;
}

fn two(input: Input, path: std.ArrayList(Point)) u32 {
    var sum: u32 = 0;
    for (path.items) |p| {
        const cost = input.grid[@intCast(p.y)][@intCast(p.x)].cost;
        for (0..21) |i| {
            const x: i32 = @intCast(i);
            if (input.tryIndex(p.add(.{ .x = x, .y = 0 }))) |t| {
                if (t.cost - cost - x >= 100) {
                    sum += 1;
                }
            }
            if (input.tryIndex(p.add(.{ .x = -x, .y = 0 }))) |t| {
                if (t.cost - cost - x >= 100) {
                    sum += 1;
                }
            }
            for (1..21 - i) |j| {
                const y: i32 = @intCast(j);
                if (input.tryIndex(p.add(.{ .x = x, .y = y }))) |t| {
                    if (t.cost - cost - x - y >= 100) {
                        sum += 1;
                    }
                }
                if (input.tryIndex(p.add(.{ .x = x, .y = -y }))) |t| {
                    if (t.cost - cost - x - y >= 100) {
                        sum += 1;
                    }
                }
                if (x != 0) {
                    if (input.tryIndex(p.add(.{ .x = -x, .y = y }))) |t| {
                        if (t.cost - cost - x - y >= 100) {
                            sum += 1;
                        }
                    }
                    if (input.tryIndex(p.add(.{ .x = -x, .y = -y }))) |t| {
                        if (t.cost - cost - x - y >= 100) {
                            sum += 1;
                        }
                    }
                }
            }
        }
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
    const steps = try solveInitial(input);
    std.debug.print("{}\n", .{one(input, steps)});
    std.debug.print("{}\n", .{two(input, steps)});
}
