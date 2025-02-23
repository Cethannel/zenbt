const std = @import("std");
const testing = std.testing;

const native_endian = @import("builtin").target.cpu.arch.endian();

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
        items: []Node,
    },
    Compound: []NamedTag,

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
                    break :blk ba.len;
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

pub fn parseFromReader(reader: anytype, allocator: std.mem.Allocator) !NamedTag {
    var arenaAllocator = std.heap.ArenaAllocator.init(allocator);
    errdefer arenaAllocator.deinit();

    return parseNamedTag(reader, allocator);
}

const endName = "End";

fn parseNamedTag(reader: anytype, allocator: std.mem.Allocator) anyerror!NamedTag {
    const tagByte: u8 = try reader.readByte();

    const tag = try std.meta.intToEnum(TAG, tagByte);

    if (tag == .End) {
        return NamedTag{
            .name = endName,
            .tag = .End,
        };
    }

    const name = try parseAssumeString(reader, allocator);
    const nameStr = name.String;
    errdefer allocator.free(nameStr);

    const node: Node = try parseNodeWithTag(reader, allocator, tag);

    return NamedTag{
        .name = nameStr,
        .tag = node,
    };
}

fn parseNodeWithTag(reader: anytype, allocator: std.mem.Allocator, tag: TAG) !Node {
    switch (tag) {
        .Byte => {
            return parseAssumeByte(reader);
        },
        .Short => {
            return parseAssumeShort(reader);
        },
        .Int => {
            return parseAssumeInt(reader);
        },
        .Long => {
            return parseAssumeLong(reader);
        },
        .Float => {
            return parseAssumeFloat(reader);
        },
        .Double => {
            return parseAssumeDouble(reader);
        },
        .ByteArray => {
            return parseAssumeByteArray(reader, allocator);
        },
        .String => {
            return parseAssumeString(reader, allocator);
        },
        .End => {
            return .End;
        },
        .Compound => {
            return parseAssumeCompound(reader, allocator);
        },
        .List => {
            return parseAssumeList(reader, allocator);
        },
    }
    unreachable;
}

pub fn parseList(reader: anytype, allocator: std.mem.Allocator) !Node {
    try checkConsumeTag(reader, TAG.List);
    return parseAssumeList(reader, allocator);
}

pub fn parseAssumeList(reader: anytype, allocator: std.mem.Allocator) !Node {
    const listTagByte: u8 = try reader.readByte();
    const listTag = try std.meta.intToEnum(TAG, listTagByte);
    const len = try parseAssumeInt(reader);
    const uLen: usize = @intCast(len.Int);
    var items = try std.ArrayList(Node).initCapacity(allocator, uLen);
    errdefer items.deinit();
    for (0..uLen) |_| {
        items.appendAssumeCapacity(try parseNodeWithTag(reader, allocator, listTag));
    }
    return Node{
        .List = .{
            .tag = listTag,
            .items = try items.toOwnedSlice(),
        },
    };
}

pub fn parseFromBytes(
    input: []const u8,
    allocator: std.mem.Allocator,
) !NamedTag {
    var stream = std.io.fixedBufferStream(input);
    return parseFromReader(stream.reader(), allocator);
}

fn parseNode(
    reader: anytype,
    allocator: std.mem.Allocator,
) !Node {
    const tagByte: u8 = try reader.readByte();

    const tag = try std.meta.intToEnum(TAG, tagByte);

    return parseNodeWithTag(reader, allocator, tag);
}

fn parseAssumeByte(reader: anytype) !Node {
    const byte: u8 = try reader.readByte();
    return Node{
        .Byte = @bitCast(byte),
    };
}

fn parseByteArray(
    reader: anytype,
    allocator: std.mem.Allocator,
) !Node {
    try checkConsumeTag(reader, TAG.ByteArray);
    return parseAssumeByteArray(reader, allocator);
}

fn checkConsumeTag(reader: anytype, expectedTag: TAG) !void {
    const tag: u8 = try reader.readByte();
    if (tag != @intFromEnum(expectedTag)) {
        return error.WrongTag;
    }
}

fn parseAssumeByteArray(reader: anytype, allocator: std.mem.Allocator) !Node {
    const len = try parseAssumeInt(reader);
    const uLen: usize = @intCast(len.Int);
    var arr = try allocator.alloc(u8, uLen);
    errdefer allocator.free(arr);
    try readFullBuffer(reader, arr[0..]);

    return Node{
        .ByteArray = arr,
    };
}

fn parseString(reader: anytype, allocator: std.mem.Allocator) !Node {
    try checkConsumeTag(reader, TAG.String);
    return parseAssumeString(reader, allocator);
}

test parseString {
    const alloc = std.testing.allocator;
    const str = "test string";
    const input = [_]u8{@intFromEnum(TAG.String)} //
    ++ comptime intToBytesBigEndian(@as(i16, str.len)) ++ str;
    var stream = std.io.fixedBufferStream(input);
    const out = try parseString(stream.reader(), alloc);
    defer alloc.free(out.String);
    const expected = Node{
        .String = str[0..],
    };
    try std.testing.expectEqualDeep(expected, out);
}

/// Turns reading less than the buffers length into an error
fn readFullBuffer(reader: anytype, buffer: []u8) !void {
    const lenRead: usize = try reader.readAll(buffer);
    if (lenRead < buffer.len) {
        return error.EndOfStream;
    }
}

fn parseAssumeString(
    reader: anytype,
    allocator: std.mem.Allocator,
) !Node {
    const len = try parseAssumeShort(reader);

    if (len.Short < 0) {
        return error.NegativeLength;
    }

    const ulen: usize = @intCast(len.Short);

    var str = try allocator.alloc(u8, ulen);
    errdefer allocator.free(str);

    try readFullBuffer(reader, str[0..]);

    return Node{
        .String = str,
    };
}

fn parseAssumeCompound(reader: anytype, allocator: std.mem.Allocator) !Node {
    var namedTags = std.ArrayList(NamedTag).init(allocator);
    errdefer namedTags.deinit();

    var namedTag = try parseNamedTag(reader, allocator);
    while (namedTag.tag != .End) : (namedTag = try parseNamedTag(reader, allocator)) {
        try namedTags.append(namedTag);
    }

    return Node{
        .Compound = try namedTags.toOwnedSlice(),
    };
}

fn parseShort(input: []const u8) !Node {
    if (input.len < 3) {
        return error.EndOfStream;
    }
    if (input[0] != @intFromEnum(TAG.Short)) {
        return error.NotShort;
    }
    return parseAssumeShort(input[1..]);
}

fn parseAssumeShort(reader: anytype) !Node {
    const short = try readTBigEndian(i16, reader);
    return Node{
        .Short = short,
    };
}

fn parseInt(reader: anytype) !Node {
    const tag: u8 = try reader.readByte();
    if (tag != @intFromEnum(TAG.Int)) {
        return error.ExpectedInt;
    }
    return parseAssumeInt(reader);
}

fn parseAssumeInt(reader: anytype) !Node {
    const int = try readTBigEndian(i32, reader);
    return Node{
        .Int = int,
    };
}

fn parseAssumeLong(reader: anytype) !Node {
    const long = try readTBigEndian(i64, reader);
    return Node{
        .Long = long,
    };
}

fn parseAssumeFloat(reader: anytype) !Node {
    const float = try readTBigEndian(f32, reader);
    return Node{
        .Float = float,
    };
}

fn parseAssumeDouble(reader: anytype) !Node {
    const double = try readTBigEndian(f64, reader);
    return Node{
        .Double = double,
    };
}

fn readTBigEndian(comptime T: type, reader: anytype) !T {
    var bytes: [@sizeOf(T)]u8 = undefined;
    try readFullBuffer(reader, bytes[0..]);
    return bytesToTBigEndian(T, bytes[0..]);
}

fn bytesToTBigEndian(comptime T: type, input: []const u8) !T {
    if (input.len < @sizeOf(T)) {
        return error.TooFewBytes;
    }
    const intT = @Type(std.builtin.Type{ .int = .{
        .signedness = .unsigned,
        .bits = @bitSizeOf(T),
    } });

    var out: intT = std.mem.bytesToValue(intT, input[0..@sizeOf(T)]);

    if (native_endian == .little) {
        out = @byteSwap(out);
    }

    return @bitCast(out);
}

fn intToBytesBigEndian(input: anytype) [@sizeOf(@TypeOf(input))]u8 {
    var swappedInput = input;
    if (native_endian == .little) {
        swappedInput = @byteSwap(swappedInput);
    }

    return std.mem.toBytes(swappedInput);
}
