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
};

pub fn parseFromBytes(
    input: []const u8,
    allocator: std.mem.Allocator,
) !NamedTag {
    std.log.info("Parsgin from: {any}", .{input});
    var arenaAlloc = std.heap.ArenaAllocator.init(allocator);
    errdefer arenaAlloc.deinit();
    return parseNamedTagFromBytesInner(input, arenaAlloc.allocator());
}

fn parseNamedTagFromBytesInner(
    input: []const u8,
    allocator: std.mem.Allocator,
) !NamedTag {
    if (input.len == 0) {
        return error.EndOfStream;
    }

    const tag = try std.meta.intToEnum(TAG, input[0]);
    std.log.info("Parsing tag: {s}", .{@tagName(tag)});

    const name = try parseAssumeString(input[2..], allocator);
    const nameStr = name.String;
    errdefer allocator.free(nameStr);

    const afterName = input[2 + name.length() ..];
    std.log.info("After name: {any}", .{afterName});

    switch (tag) {
        .Byte => {
            const byte = try bytesToIntBigEndian(
                i8,
                afterName,
            );
            return NamedTag{
                .name = nameStr,
                .tag = .{ .Byte = byte },
            };
        },
        .Short => {
            const short = try bytesToIntBigEndian(
                i16,
                afterName,
            );
            return NamedTag{
                .name = nameStr,
                .tag = .{ .Short = short },
            };
        },
        .Int => {
            const int = try bytesToIntBigEndian(
                i32,
                afterName,
            );
            return NamedTag{
                .name = nameStr,
                .tag = .{ .Int = int },
            };
        },
        .Long => {
            const long = try bytesToIntBigEndian(
                i64,
                afterName,
            );
            return NamedTag{
                .name = nameStr,
                .tag = .{ .Long = long },
            };
        },
        .Float => {
            const float = try bytesToIntBigEndian(
                f32,
                afterName,
            );
            return NamedTag{
                .name = nameStr,
                .tag = .{ .Float = float },
            };
        },
        .Double => {
            const double = try bytesToIntBigEndian(
                f64,
                afterName,
            );
            return NamedTag{
                .name = nameStr,
                .tag = .{ .Double = double },
            };
        },
        .ByteArray => {
            const byteArray = try parseByteArray(afterName, allocator);
            return NamedTag{
                .name = nameStr,
                .tag = byteArray,
            };
        },
        .String => {
            const string = try parseString(afterName, allocator);
            return NamedTag{
                .name = nameStr,
                .tag = string,
            };
        },
        .End => {
            unreachable;
        },
        .Compound => {
            var entries = std.ArrayList(NamedTag).init(allocator);
            errdefer entries.deinit();
            var cur = afterName[0..];
            while (cur.len > 0 and cur[0] != @intFromEnum(TAG.End)) {
                const namedTag = try parseNamedTagFromBytesInner(cur, allocator);
                try entries.append(namedTag);
                cur = cur[namedTag.length()..];
            }
            return NamedTag{
                .name = nameStr,
                .tag = .{ .Compound = try entries.toOwnedSlice() },
            };
        },
        .List => {},
    }
    unreachable;
}

fn parseNodeFromBytes(
    input: []const u8,
    allocator: std.mem.Allocator,
) !Node {
    if (input.len <= 0) {
        return error.EndOfStream;
    }

    const tag = try std.meta.intToEnum(TAG, input[0]);
    const afterTag = input[1..];

    switch (tag) {
        .Byte => {
            const byte = try bytesToIntBigEndian(
                i8,
                afterTag,
            );
            return .{ .Byte = byte };
        },
        .Short => {
            const short = try bytesToIntBigEndian(
                i16,
                afterTag,
            );
            return .{ .Short = short };
        },
        .Int => {
            const int = try bytesToIntBigEndian(
                i32,
                afterTag,
            );
            return .{ .Int = int };
        },
        .Long => {
            const long = try bytesToIntBigEndian(
                i64,
                afterTag,
            );
            return .{ .Long = long };
        },
        .Float => {
            const float = try bytesToIntBigEndian(
                f32,
                afterTag,
            );
            return .{ .Float = float };
        },
        .Double => {
            const double = try bytesToIntBigEndian(
                f64,
                afterTag,
            );
            return .{ .Double = double };
        },
        .ByteArray => {
            const byteArray = try parseByteArray(afterTag, allocator);
            return byteArray;
        },
        .String => {
            const string = try parseString(afterTag, allocator);
            return string;
        },
        .End => {
            unreachable;
        },
        .Compound => {
            var entries = std.ArrayList(Node).init(allocator);
            errdefer entries.deinit();
            var cur = afterTag[0..];
            while (cur.len > 0 and cur[0] != @intFromEnum(TAG.End)) {
                const namedTag = try parseNamedTagFromBytesInner(cur, allocator);
                try entries.append(namedTag);
                cur = cur[namedTag.length()..];
            }
            return .{ .Compound = try entries.toOwnedSlice() };
        },
        .List => {},
    }
}

fn parseByteArray(
    input: []const u8,
    allocator: std.mem.Allocator,
) !Node {
    if (input.len < 1 + (Node{ .Int = 0 }).length()) {
        return error.EndOfStream;
    }

    const len = try parseInt(input[1..]);
    const arrBuf = input[3..];
    if (arrBuf.len < len.Int) {
        return error.EndOfStream;
    }
    const uLen: usize = @intCast(len.Int);

    var arr = try allocator.alloc(u8, uLen);
    errdefer allocator.free(arr);

    @memcpy(arr[0..], arrBuf[0..uLen]);

    return Node{
        .ByteArray = arr,
    };
}

fn parseString(input: []const u8, allocator: std.mem.Allocator) !Node {
    if (input.len < 3) {
        return error.EndOfStream;
    }
    if (input[0] != @intFromEnum(TAG.String)) {
        return error.NotString;
    }
    return parseAssumeString(input[1..], allocator);
}

test parseString {
    const alloc = std.testing.allocator;
    const str = "test string";
    const input = [_]u8{@intFromEnum(TAG.String)} //
    ++ comptime intToBytesBigEndian(@as(i16, str.len)) ++ str;
    const out = try parseString(input, alloc);
    defer alloc.free(out.String);
    const expected = Node{
        .String = str[0..],
    };
    try std.testing.expectEqualDeep(expected, out);
}

fn parseAssumeString(
    input: []const u8,
    allocator: std.mem.Allocator,
) !Node {
    const len = try parseAssumeShort(input);

    if (len.Short < 0) {
        return error.NegativeLength;
    }

    const ulen: usize = @intCast(len.Short);

    const strBuf = input[2..];
    if (strBuf.len < ulen) {
        std.log.err("Too short for len: {}", .{ulen});
        std.log.err("Is len: {}", .{strBuf.len});
        return error.EndOfStream;
    }

    var str = try allocator.alloc(u8, ulen);
    errdefer allocator.free(str);

    @memcpy(str[0..], strBuf[0..ulen]);

    return Node{
        .String = str,
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

fn parseAssumeShort(input: []const u8) !Node {
    if (input.len < 2) {
        return error.EndOfStream;
    }
    const short = try bytesToIntBigEndian(i16, input);
    return Node{
        .Short = short,
    };
}

fn parseInt(input: []const u8) !Node {
    if (input.len < (Node{ .Int = 0 }).length()) {
        return error.EndOfStream;
    }
    if (input[0] != @intFromEnum(TAG.Int)) {
        return error.NotInt;
    }
    const short = try bytesToIntBigEndian(i32, input[1..]);
    return Node{
        .Int = short,
    };
}

fn bytesToIntBigEndian(comptime T: type, input: []const u8) !T {
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
