const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const NamedTag = ast.NamedTag;
const Node = ast.Node;
const TAG = ast.TAG;

const parser = @import("parser.zig");
pub const deserializer = @import("deserializer.zig");

pub const parseFromBytes = parser.parseFromBytes;

pub const SerializeOptions = struct {
    printErrors: bool = false,
    trace: bool = false,

    pub fn logErr(
        self: *const @This(),
        comptime format: []const u8,
        args: anytype,
    ) void {
        if (self.printErrors) {
            std.log.err(format, args);
        }
    }

    pub fn logTrace(
        self: *const @This(),
        comptime format: []const u8,
        args: anytype,
    ) void {
        if (self.trace) {
            std.debug.print(format, args);
            std.debug.print("\r\n", .{});
        }
    }
};

pub fn serializeNamedTagAlloc(
    allocator: std.mem.Allocator,
    namedTag: NamedTag,
    options: SerializeOptions,
) ![]const u8 {
    options.logTrace("Serializing with alloc", .{});
    var out = std.ArrayList(u8).init(allocator);
    errdefer out.deinit();
    try serializeNamedTag(namedTag, out.writer(), options);
    return out.toOwnedSlice();
}

pub fn serializeNamedTag(
    namedTag: NamedTag,
    writer: anytype,
    options: SerializeOptions,
) anyerror!void {
    options.logTrace("Serializing named tag: `{s}`", .{namedTag.name});
    try writer.writeByte(@intFromEnum(namedTag.tag));
    try serializeStringNoTag(namedTag.name, writer, options);
    try serializeNode(namedTag.tag, writer, options);
}

pub fn serializeNode(
    node: Node,
    writer: anytype,
    options: SerializeOptions,
) !void {
    options.logTrace("Serializing node of tag: `{s}`", .{@tagName(node)});
    switch (node) {
        .Byte => |b| {
            try writer.writeInt(i8, @intCast(b), std.builtin.Endian.big);
        },
        .Short => |s| {
            try writer.writeInt(i16, @intCast(s), std.builtin.Endian.big);
        },
        .Int => |i| {
            try writer.writeInt(i32, @intCast(i), std.builtin.Endian.big);
        },
        .Long => |l| {
            try writer.writeInt(i64, @intCast(l), std.builtin.Endian.big);
        },
        .Float => |f| {
            const i: i32 = @bitCast(f);
            try writer.writeInt(i32, @intCast(i), std.builtin.Endian.big);
        },
        .Double => |d| {
            const i: i64 = @bitCast(d);
            try writer.writeInt(i64, @intCast(i), std.builtin.Endian.big);
        },
        .ByteArray => |ba| {
            try writer.writeInt(i32, @intCast(ba.len), std.builtin.Endian.big);
            try writer.writeAll(ba);
        },
        .String => |s| {
            try writer.writeInt(i16, @intCast(s.len), std.builtin.Endian.big);
            try writer.writeAll(s);
        },
        .List => |l| {
            try writer.writeByte(@intFromEnum(l.tag));
            try writer.writeInt(i32, @intCast(l.items.len), std.builtin.Endian.big);
            for (l.items) |item| {
                try serializeNode(item, writer, options);
            }
        },
        .Compound => |c| {
            for (c) |nt| {
                try serializeNamedTag(nt, writer, options);
            }
            try writer.writeByte(@intFromEnum(TAG.End));
        },
        .End => {
            unreachable;
        },
        .IntArray => |ia| {
            try writer.writeInt(i32, @intCast(ia.len), std.builtin.Endian.big);
            for (ia) |i| {
                try writer.writeInt(i32, i, std.builtin.Endian.big);
            }
        },
        .LongArray => |la| {
            try writer.writeInt(i32, @intCast(la.len), std.builtin.Endian.big);
            for (la) |i| {
                try writer.writeInt(i64, i, std.builtin.Endian.big);
            }
        },
    }
}

test "Round trip test" {
    const allocator = std.testing.allocator;
    const servers = @embedFile("./testFiles/servers.dat");
    var arenaAlloc = std.heap.ArenaAllocator.init(allocator);
    defer arenaAlloc.deinit();
    const nt = try parseFromBytes(servers, arenaAlloc.allocator(), .{ .printErrors = true });
    const out = try serializeNamedTagAlloc(allocator, nt, .{});
    defer allocator.free(out);
    try std.testing.expectEqualSlices(u8, servers, out);

    const store_parser = @import("store_parser.zig");

    std.testing.refAllDecls(ast);
    std.testing.refAllDecls(store_parser);
}

pub fn serializeStringNoTag(
    str: []const u8,
    writer: anytype,
    options: SerializeOptions,
) !void {
    _ = options;
    try writer.writeInt(i16, @intCast(str.len), std.builtin.Endian.big);
    try writer.writeAll(str);
}
