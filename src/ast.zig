const std = @import("std");
const testing = std.testing;

pub const TAG = enum(u8) {
    /// TYPE: 0  NAME: TAG_End
    /// Payload: None.
    /// Note:    This tag is used to mark the end of a list.
    ///          Cannot be named! If type 0 appears where a Named Tag is expected, the name is assumed to be "".
    ///          (In other words, this Tag is always just a single 0 byte when named, and nothing in all other cases)
    End = 0,

    /// TYPE: 1  NAME: TAG_Byte
    /// Payload: A single signed byte (8 bits)
    Byte = 1,

    /// TYPE: 2  NAME: TAG_Short
    /// Payload: A signed short (16 bits, big endian)
    Short = 2,

    /// TYPE: 3  NAME: TAG_Int
    /// Payload: A signed short (32 bits, big endian)
    Int = 3,

    /// TYPE: 4  NAME: TAG_Long
    /// Payload: A signed long (64 bits, big endian)
    Long = 4,

    /// TYPE: 5  NAME: TAG_Float
    /// Payload: A floating point value (32 bits, big endian, IEEE 754-2008, binary32)
    Float = 5,

    /// TYPE: 6  NAME: TAG_Double
    /// Payload: A floating point value (64 bits, big endian, IEEE 754-2008, binary64)
    Double = 6,

    /// TYPE: 7  NAME: TAG_Byte_Array
    /// Payload: TAG_Int length
    ///          An array of bytes of unspecified format. The length of this array is <length> bytes
    ByteArray = 7,

    /// TYPE: 8  NAME: TAG_String
    /// Payload: TAG_Short length
    ///          An array of bytes defining a string in UTF-8 format. The length of this array is <length> bytes
    String = 8,

    /// TYPE: 9  NAME: TAG_List
    /// Payload: TAG_Byte tagId
    ///          TAG_Int length
    ///          A sequential list of Tags (not Named Tags), of type <typeId>. The length of this array is <length> Tags
    /// Notes:   All tags share the same type.
    List = 9,

    /// TYPE: 10 NAME: TAG_Compound
    /// Payload: A sequential list of Named Tags. This array keeps going until a TAG_End is found.
    ///          TAG_End end
    /// Notes:   If there's a nested TAG_Compound within this tag, that one will also have a TAG_End, so simply reading until the next TAG_End will not work.
    ///          The names of the named tags have to be unique within each TAG_Compound
    ///          The order of the tags is not guaranteed.
    Compound = 10,

    /// TYPE: 11 NAME: TAG_Int_Array
    /// Payload: TAG_Int length
    ///          An array of ints of unspecified format. The length of this array is <length> bytes
    IntArray = 11,

    /// TYPE: 12 NAME: TAG_Long_Array
    /// Payload: TAG_Int length
    ///          An array of ints of unspecified format. The length of this array is <length> bytes
    LongArray = 12,
};

pub const Node = union(TAG) {
    const Self = @This();
    End,
    Byte: i8,
    Short: i16,
    Int: i32,
    Long: i64,
    Float: f32,
    Double: f64,
    ByteArray: []const u8,
    String: []const u8,
    List: struct {
        tag: TAG,
        items: []const Node,
    },
    Compound: []const NamedTag,
    IntArray: []const i32,
    LongArray: []const i64,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        try writer.print("TAG_{s}: ", .{@tagName(self)});
        switch (self) {
            .End => return,
            .Byte => |b| try writer.print(
                "{}",
                .{b},
            ),
            .Short => |s| try writer.print(
                "{}",
                .{s},
            ),
            .Int => |i| try writer.print(
                "{}",
                .{i},
            ),
            .Long => |l| try writer.print(
                "{}",
                .{l},
            ),
            .Float => |f| try writer.print(
                "{}",
                .{f},
            ),
            .Double => |d| try writer.print(
                "{}",
                .{d},
            ),
            .ByteArray => |ba| try writer.print(
                "{any}",
                .{ba},
            ),
            .String => |s| try writer.print(
                "{s}",
                .{s},
            ),
            .List => |s| try writer.print(
                "{}",
                .{s},
            ),
            .Compound => |c| {
                for (c) |value| {
                    try writer.print("{}\n", .{value});
                }
            },
            .IntArray => |ia| {
                try writer.print(
                    "{any}",
                    .{ia},
                );
            },
            .LongArray => |la| {
                try writer.print(
                    "{any}",
                    .{la},
                );
            },
        }
    }

    pub fn length(self: *const Self) usize {
        const lenNoByte = blk: {
            switch (self.*) {
                .End => {
                    break :blk 0;
                },
                .Byte => {
                    break :blk @sizeOf(i8);
                },
                .Short => {
                    break :blk @sizeOf(i16);
                },
                .Int => {
                    break :blk @sizeOf(i32);
                },
                .Long => {
                    break :blk @sizeOf(i64);
                },
                .Float => {
                    break :blk @sizeOf(f32);
                },
                .Double => {
                    break :blk @sizeOf(f64);
                },
                .ByteArray => |ba| {
                    break :blk @sizeOf(i32) + ba.len;
                },
                .String => |str| {
                    break :blk str.len + 2;
                },
                .List => |lst| {
                    var out: usize = 1 + (Node{ .Int = 0 }).length();
                    for (lst.items) |value| {
                        out += value.length();
                    }
                    break :blk out;
                },
                .Compound => |cmp| {
                    var out: usize = 1;
                    for (cmp) |value| {
                        out += value.length();
                    }
                    break :blk out;
                },
                .IntArray => |ia| {
                    break :blk @sizeOf(i32) * (ia.len + 1);
                },
                .LongArray => |la| {
                    break :blk @sizeOf(i32) + @sizeOf(i64) * la.len;
                },
            }
            unreachable;
        };

        return lenNoByte + 1;
    }

    test length {
        const tests = [_]struct {
            node: Self,
            expectedLength: usize,
        }{
            .{
                .node = .End,
                .expectedLength = 1,
            },
            .{
                .node = .{ .Byte = undefined },
                .expectedLength = 2,
            },
            .{
                .node = .{ .Short = undefined },
                .expectedLength = 3,
            },
            .{
                .node = .{ .Int = undefined },
                .expectedLength = 5,
            },
            .{
                .node = .{ .Long = undefined },
                .expectedLength = 9,
            },
            .{
                .node = .{ .Float = undefined },
                .expectedLength = 5,
            },
            .{
                .node = .{ .Double = undefined },
                .expectedLength = 9,
            },
            .{
                .node = .{ .String = "hello world" },
                .expectedLength = 1 + 2 + "hello world".len,
            },
            .{
                .node = .{ .ByteArray = &[_]u8{ 1, 2, 3, 4, 5, 6, 7 } },
                .expectedLength = 1 + @sizeOf(i32) + @sizeOf(i8) * 7,
            },
            .{
                .node = .{ .IntArray = &[_]i32{ 1, 2, 3, 4, 5, 6, 7 } },
                .expectedLength = 1 + @sizeOf(i32) + @sizeOf(i32) * 7,
            },
            .{
                .node = .{ .LongArray = &[_]i64{ 1, 2, 3, 4, 5, 6, 7 } },
                .expectedLength = 1 + @sizeOf(i32) + @sizeOf(i64) * 7,
            },
        };

        for (tests) |tt| {
            try std.testing.expectEqual(
                tt.expectedLength,
                tt.node.length(),
            );
        }
    }

    fn fancyPrintInner(self: *const Self, writer: anytype, ctx: fancyPrintCtx) anyerror!void {
        switch (self.*) {
            .End => return,
            .ByteArray => |ba| {
                _ = try writer.print("[{} bytes]", .{ba.len});
            },
            .List => |l| {
                try writer.print("{} entries of type TAG_{s}\r\n", .{
                    l.items.len,
                    @tagName(l.tag),
                });
                try writer.writeBytesNTimes(ctx.indentStr, ctx.depth);
                _ = try writer.write("{\r\n");

                var newCtx = ctx;
                newCtx.depth += 1;
                for (l.items) |node| {
                    try writer.writeBytesNTimes(ctx.indentStr, ctx.depth + 1);
                    try writer.print("TAG_{s}: ", .{@tagName(l.tag)});
                    try node.fancyPrintInner(writer, newCtx);
                    _ = try writer.write("\r\n");
                }

                try writer.writeBytesNTimes(ctx.indentStr, ctx.depth);
                _ = try writer.write("}");
            },
            .Compound => |c| {
                try writer.print("{} entries\r\n", .{
                    c.len,
                });
                try writer.writeBytesNTimes(ctx.indentStr, ctx.depth);
                _ = try writer.write("{\r\n");

                var newCtx = ctx;
                newCtx.depth += 1;
                for (c) |namedTag| {
                    try namedTag.fancyPrintInner(writer, newCtx);
                    _ = try writer.write("\r\n");
                }

                try writer.writeBytesNTimes(ctx.indentStr, ctx.depth);
                _ = try writer.write("}");
            },
            else => {
                _ = try writer.print("{}", .{self});
            },
        }
    }
};

pub const NamedTag = struct {
    const Self = @This();
    name: []const u8,
    tag: Node,

    pub fn format(
        self: @This(),
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (self.tag != .End) {
            try writer.print("TAG_{s}(\"{s}\"): ", .{ @tagName(self.tag), self.name });
        }
        switch (self.tag) {
            .End => return,
            .Byte => |b| try writer.print(
                "{}",
                .{b},
            ),
            .Short => |s| try writer.print(
                "{}",
                .{s},
            ),
            .Int => |i| try writer.print(
                "{}",
                .{i},
            ),
            .Long => |l| try writer.print(
                "{}",
                .{l},
            ),
            .Float => |f| try writer.print(
                "{}",
                .{f},
            ),
            .Double => |d| try writer.print(
                "{}",
                .{d},
            ),
            .ByteArray => |ba| try writer.print(
                "{any}",
                .{ba},
            ),
            .String => |s| try writer.print(
                "{s}",
                .{s},
            ),
            .List => |l| {
                try writer.print("{} entries of type TAG_{s}\n{{\n", .{ l.items.len, @tagName(l.tag) });
                for (l.items) |value| {
                    try writer.print("{}\n", .{value});
                }
                try writer.print("}}\n", .{});
            },
            .Compound => |c| {
                try writer.print("{} entries\n{{\n", .{c.len});
                for (c) |value| {
                    try writer.print("{}\n", .{value});
                }
                try writer.print("}}\n", .{});
            },
            .IntArray => |ia| {
                try writer.print("{any}", .{ia});
            },
            .LongArray => |ia| {
                try writer.print("{any}", .{ia});
            },
        }
    }

    pub fn length(self: *const Self) usize {
        return (Node{ .String = self.name }).length() + self.tag.length();
    }

    pub fn fancyPrintStdOut(self: *const Self) !void {
        var stdout = std.io.getStdOut();
        defer stdout.close();
        try self.fancyPrint(stdout.writer());
    }

    pub fn fancyPrint(self: *const Self, writer: anytype) !void {
        try self.fancyPrintInner(writer, .{});
    }

    fn fancyPrintInner(self: *const Self, writer: anytype, ctx: fancyPrintCtx) !void {
        try writer.writeByteNTimes('\t', ctx.depth);
        if (self.tag != .End) {
            try writer.print("TAG_{s}(\"{s}\"): ", .{ @tagName(self.tag), self.name });
        }
        try self.tag.fancyPrintInner(writer, ctx);
    }
};

const fancyPrintCtx = struct {
    depth: usize = 0,
    indentStr: []const u8 = "\t",
};
