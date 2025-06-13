const ast = @import("ast.zig");
const std = @import("std");

const AstStore = ast.AstStore;
const TAG = ast.TAG;

const native_endian = @import("builtin").target.cpu.arch.endian();

pub const Parser = struct {
    arena: std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    store: ast.AstStore,

    const Self = @This();

    pub fn init(child_allocator: std.mem.Allocator) Self {
        const arena = std.heap.ArenaAllocator.init(child_allocator);
        const allocator = arena.allocator();

        return Self{
            .arena = arena,
            .allocator = allocator,
            .store = .init(allocator),
        };
    }

    fn checkConsumeTag(reader: anytype, expectedTag: TAG) !void {
        const tag: u8 = try reader.readByte();
        if (tag != @intFromEnum(expectedTag)) {
            return error.WrongTag;
        }
    }

    fn parseAssumeShort(reader: anytype) !i16 {
        const short = try readTBigEndian(i16, reader);
        return short;
    }

    pub fn parseAssumeString(self: *Self, reader: anytype) !AstStore.NodeIndex {
        const len = try parseAssumeShort(reader);

        if (len.Short < 0) {
            return error.NegativeLength;
        }

        const ulen: usize = @intCast(len.Short);

        var str = try self.allocator.alloc(u8, ulen);
        defer self.allocator.free(str);

        try readFullBuffer(reader, str[0..]);

        return self.store.newString(str);
    }

    pub fn parseString(self: *Self, reader: anytype) !ast.AstStore.NodeIndex {
        try checkConsumeTag(reader, .String);
        return self.parseAssumeString(reader);
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

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
    }
};

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

/// Turns reading less than the buffers length into an error
fn readFullBuffer(reader: anytype, buffer: []u8) !void {
    const lenRead: usize = try reader.readAll(buffer);
    if (lenRead < buffer.len) {
        return error.EndOfStream;
    }
}

fn intToBytesBigEndian(input: anytype) [@sizeOf(@TypeOf(input))]u8 {
    var swappedInput = input;
    if (native_endian == .little) {
        swappedInput = @byteSwap(swappedInput);
    }

    return std.mem.toBytes(swappedInput);
}
