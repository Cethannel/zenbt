const std = @import("std");
const testing = std.testing;

const ast = @import("ast.zig");
const NamedTag = ast.NamedTag;
const Node = ast.Node;
const TAG = ast.TAG;

const deserializer = @import("deserializer.zig");
const Parsed = deserializer.Parsed;

const native_endian = @import("builtin").target.cpu.arch.endian();

const readEndian = std.builtin.Endian.big;

pub fn parseFromReader(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
    options: ParseOptions,
) !NamedTag {
    var arenaAllocator = std.heap.ArenaAllocator.init(allocator);
    errdefer arenaAllocator.deinit();

    return parseNamedTag(reader, allocator, options);
}

const endName = "End";

fn parseNamedTag(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
    options: ParseOptions,
) anyerror!NamedTag {
    const tagByte: u8 = try reader.takeByte();

    const tag = std.meta.intToEnum(TAG, tagByte) catch |err| {
        options.logErr("Unknown tag: {}", .{tagByte});
        return err;
    };

    if (tag == .End) {
        return NamedTag{
            .name = endName,
            .tag = .End,
        };
    }

    const name = try parseAssumeString(reader, allocator);
    const nameStr = name.String;
    errdefer allocator.free(nameStr);

    const node: Node = try parseNodeWithTag(reader, allocator, options, tag);

    return NamedTag{
        .name = nameStr,
        .tag = node,
    };
}

fn parseNodeWithTag(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
    options: ParseOptions,
    tag: TAG,
) !Node {
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
            return parseAssumeCompound(reader, allocator, options);
        },
        .List => {
            return parseAssumeList(reader, allocator, options);
        },
        .IntArray => {
            return parseAssumeIntArray(reader, allocator, options);
        },
        .LongArray => {
            return parseAssumeLongArray(reader, allocator, options);
        },
    }
    unreachable;
}

pub fn parseList(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
    options: ParseOptions,
) !Node {
    try checkConsumeTag(reader, TAG.List);
    return parseAssumeList(reader, allocator, options);
}

pub fn parseAssumeList(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
    options: ParseOptions,
) !Node {
    const listTagByte: u8 = try reader.takeByte();
    const listTag = try std.meta.intToEnum(TAG, listTagByte);
    const len = try parseAssumeInt(reader);
    const uLen: usize = @intCast(len.Int);
    var items = try allocator.alloc(Node, uLen);
    errdefer allocator.free(items);
    for (0..uLen) |i| {
        items[i] = try parseNodeWithTag(reader, allocator, options, listTag);
    }
    return Node{
        .List = .{
            .tag = listTag,
            .items = items,
        },
    };
}

pub const ParseOptions = struct {
    printErrors: bool = false,

    pub fn logErr(
        self: *const @This(),
        comptime format: []const u8,
        args: anytype,
    ) void {
        if (self.printErrors) {
            std.log.err(format, args);
        }
    }
};

pub fn parseFromBytes(
    input: []const u8,
    allocator: std.mem.Allocator,
    options: ParseOptions,
) !NamedTag {
    var stream = std.io.Reader.fixed(input);
    return parseFromReader(&stream, allocator, options);
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
    _ = input;
    _ = allocator;
    unreachable;
    //switch () {}
}

fn parseNode(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
    options: ParseOptions,
) !Node {
    const tagByte: u8 = try reader.takeByte();

    const tag = try std.meta.intToEnum(TAG, tagByte);

    return parseNodeWithTag(reader, allocator, options, tag);
}

fn parseAssumeByte(reader: *std.io.Reader) !Node {
    const byte: u8 = try reader.takeByte();
    return Node{
        .Byte = @bitCast(byte),
    };
}

fn parseByteArray(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
) !Node {
    try checkConsumeTag(reader, TAG.ByteArray);
    return parseAssumeByteArray(reader, allocator);
}

fn checkConsumeTag(reader: *std.io.Reader, expectedTag: TAG) !void {
    const tag: u8 = try reader.takeByte();
    if (tag != @intFromEnum(expectedTag)) {
        return error.WrongTag;
    }
}

fn parseAssumeByteArray(reader: *std.io.Reader, allocator: std.mem.Allocator) !Node {
    const len = try parseAssumeInt(reader);
    const uLen: usize = @intCast(len.Int);
    const arr = try reader.readAlloc(allocator, uLen);
    errdefer allocator.free(arr);

    return Node{
        .ByteArray = arr,
    };
}

fn parseAssumeIntArray(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
    options: ParseOptions,
) !Node {
    _ = options;
    const len = try parseAssumeInt(reader);
    const uLen: usize = @intCast(len.Int);
    var arr = try allocator.alloc(i32, uLen);
    errdefer allocator.free(arr);
    for (0..uLen) |i| {
        arr[i] = (try parseAssumeInt(reader)).Int;
    }

    return Node{
        .IntArray = arr,
    };
}

fn parseAssumeLongArray(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
    options: ParseOptions,
) !Node {
    _ = options;
    const len = try parseAssumeInt(reader);
    const uLen: usize = @intCast(len.Int);
    var arr = try allocator.alloc(i64, uLen);
    errdefer allocator.free(arr);
    for (0..uLen) |i| {
        arr[i] = (try parseAssumeLong(reader)).Long;
    }

    return Node{
        .LongArray = arr,
    };
}

fn parseString(reader: *std.io.Reader, allocator: std.mem.Allocator) !Node {
    try checkConsumeTag(reader, TAG.String);
    return parseAssumeString(reader, allocator);
}

test parseString {
    const alloc = std.testing.allocator;
    const str = "test string";
    const input = [_]u8{@intFromEnum(TAG.String)} //
        ++ comptime intToBytesBigEndian(@as(i16, str.len)) ++ str;
    var stream = std.io.Reader.fixed(input);
    const out = try parseString(&stream, alloc);
    defer alloc.free(out.String);
    var thing: [str.len]u8 = undefined;
    @memcpy(&thing, str);
    const expected = Node{
        .String = &thing,
    };
    try std.testing.expectEqualDeep(expected, out);
}

fn parseAssumeString(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
) !Node {
    const len = try parseAssumeShort(reader);

    if (len.Short < 0) {
        return error.NegativeLength;
    }

    const ulen: usize = @intCast(len.Short);

    const str = try reader.readAlloc(allocator, ulen);
    errdefer allocator.free(str);

    return Node{
        .String = str,
    };
}

fn parseAssumeCompound(
    reader: *std.io.Reader,
    allocator: std.mem.Allocator,
    options: ParseOptions,
) !Node {
    var namedTags = std.ArrayList(NamedTag).empty;
    errdefer namedTags.deinit(allocator);

    var namedTag = try parseNamedTag(reader, allocator, options);
    while (namedTag.tag != .End) : (namedTag = try parseNamedTag(reader, allocator, options)) {
        try namedTags.append(allocator, namedTag);
    }

    return Node{
        .Compound = try namedTags.toOwnedSlice(allocator),
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

fn parseAssumeShort(reader: *std.io.Reader) !Node {
    const short = try reader.takeInt(i16, readEndian);
    return Node{
        .Short = short,
    };
}

fn parseInt(reader: *std.io.Reader) !Node {
    const tag: u8 = try reader.takeByte();
    if (tag != @intFromEnum(TAG.Int)) {
        return error.ExpectedInt;
    }
    return parseAssumeInt(reader);
}

fn parseAssumeInt(reader: *std.io.Reader) !Node {
    const int = try reader.takeInt(i32, readEndian);
    return Node{
        .Int = int,
    };
}

fn parseAssumeLong(reader: *std.io.Reader) !Node {
    const long = try reader.takeInt(i64, readEndian);
    return Node{
        .Long = long,
    };
}

fn parseAssumeFloat(reader: *std.io.Reader) !Node {
    const float = try readTBigEndian(f32, reader);
    return Node{
        .Float = float,
    };
}

fn parseAssumeDouble(reader: *std.io.Reader) !Node {
    const double = try readTBigEndian(f64, reader);
    return Node{
        .Double = double,
    };
}

fn readTBigEndian(comptime T: type, reader: *std.io.Reader) !T {
    var bytes: [@sizeOf(T)]u8 = undefined;
    try reader.readSliceAll(bytes[0..]);
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
