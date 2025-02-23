const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const NamedTag = ast.NamedTag;
const Node = ast.Node;
const TAG = ast.TAG;

const deserializer = @import("deserializer.zig");
const Parsed = deserializer.Parsed;

const native_endian = @import("builtin").target.cpu.arch.endian();

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

pub fn parseFromType(
    input: anytype,
    allocator: std.mem.Allocator,
) !Parsed(NamedTag) {
    var parsed = Parsed(NamedTag){
        .arena = try allocator.create(std.heap.ArenaAllocator),
        .value = undefined,
    };
    parsed.arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer parsed.arena.deinit();

    parsed.value = innerParseType(input, parsed.arena.allocator());

    return parsed;
}

fn innerParseType(
    input: anytype,
    allocator: std.mem.Allocator,
) !NamedTag {
    switch () {}
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
