const ast = @import("ast.zig");
const std = @import("std");

const AstStore = ast.AstStore;
const TAG = ast.TAG;

const native_endian = @import("builtin").target.cpu.arch.endian();

pub const Parser = struct {
    arena: *std.heap.ArenaAllocator,
    allocator: std.mem.Allocator,
    store: ast.AstStore,

    const Self = @This();

    pub fn init(child_allocator: std.mem.Allocator) !Self {
        const arena = try child_allocator.create(std.heap.ArenaAllocator);
        arena.* = .init(child_allocator);
        const allocator = arena.allocator();

        return Self{
            .arena = arena,
            .allocator = allocator,
            .store = try .init(allocator),
        };
    }

    pub fn parseFromBytes(
        self: *Self,
        input: []const u8,
    ) !AstStore.NamedTag {
        var stream = std.io.fixedBufferStream(input);
        return self.parseFromReader(stream.reader());
    }

    fn checkConsumeTag(reader: anytype, expectedTag: TAG) !void {
        const tag: u8 = try reader.readByte();
        if (tag != @intFromEnum(expectedTag)) {
            return error.WrongTag;
        }
    }

    fn parseAssumeShort(self: *Self, reader: anytype) !i16 {
        _ = self;
        const short = try readTBigEndian(i16, reader);
        return short;
    }

    fn parseAssumeByte(self: *Self, reader: anytype) !i8 {
        _ = self;
        const byte = try readTBigEndian(i8, reader);
        return byte;
    }

    fn parseAssumeString(self: *Self, reader: anytype) !AstStore.NodeIndex {
        const len = try self.parseAssumeShort(reader);

        if (len < 0) {
            return error.NegativeLength;
        }

        const ulen: usize = @intCast(len);

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
        var parser = try Self.init(alloc);
        defer parser.deinit();
        const out = try parser.parseString(stream.reader());
        const expected = try parser.store.newString(str);
        try parser.store.expectEqualDeep(expected, out);
    }

    pub fn parseFromReader(
        self: *Self,
        reader: anytype,
    ) !AstStore.NamedTag {
        return self.parseNamedTag(reader);
    }

    fn parseNamedTag(
        self: *Self,
        reader: anytype,
    ) !AstStore.NamedTag {
        const tagByte: u8 = try reader.readByte();

        const tag = std.meta.intToEnum(TAG, tagByte) catch |err| {
            return err;
        };

        if (tag == .End) {
            return self.store.end_named_tag;
        }

        const name = try self.parseAssumeString(reader);

        const idx = try self.parseNodeWithTag(reader, tag);

        return .{
            .idx = idx,
            .name_idx = name,
        };
    }

    fn parseNodeWithTag(
        self: *Self,
        reader: anytype,
        tag: TAG,
    ) !AstStore.NodeIndex {
        switch (tag) {
            TAG.Byte => {
                return self.store.newByte(
                    try self.parseAssumeByte(reader),
                );
            },
            TAG.Short => {
                return self.store.newShort(
                    try self.parseAssumeShort(reader),
                );
            },
            TAG.Int => {
                return self.store.newInt(
                    try self.parseAssumeInt(reader),
                );
            },
            TAG.Long => {
                return self.store.newLong(
                    try self.parseAssumeLong(reader),
                );
            },
            TAG.Double => {
                return self.store.newDouble(
                    try self.parseAssumeDouble(reader),
                );
            },
            TAG.Compound => {
                return self.parseAssumeCompound(reader);
            },
            TAG.List => {
                return self.parseAssumeList(reader);
            },
            TAG.IntArray => {
                return self.parseAssumeIntArray(reader);
            },
            TAG.String => {
                return self.parseAssumeString(reader);
            },
            else => {
                std.debug.panic("Unsupported tag: {}", .{tag});
            },
        }
    }

    fn parseAssumeIntArray(
        self: *Self,
        reader: anytype,
    ) !AstStore.NodeIndex {
        const len = try self.parseAssumeInt(reader);
        const uLen: usize = @intCast(len);
        var arr = try self.allocator.alloc(i32, uLen);
        errdefer self.allocator.free(arr);
        for (0..uLen) |i| {
            arr[i] = try self.parseAssumeInt(reader);
        }

        return self.store.newIntArray(arr);
    }

    fn parseAssumeList(
        self: *Self,
        reader: anytype,
    ) !AstStore.NodeIndex {
        const listTagByte: u8 = try reader.readByte();
        const listTag = try std.meta.intToEnum(TAG, listTagByte);
        const len = try self.parseAssumeInt(reader);

        const uLen: usize = @intCast(len);

        const out = try self.store.newList(listTag, @intCast(len));

        for (0..uLen) |i| {
            out.arr[i] = try self.parseNodeWithTag(reader, listTag);
        }

        return out.idx;
    }

    fn parseAssumeInt(
        self: *Self,
        reader: anytype,
    ) !i32 {
        _ = self;
        const int = try readTBigEndian(i32, reader);
        return int;
    }

    fn parseAssumeLong(
        self: *Self,
        reader: anytype,
    ) !i64 {
        _ = self;
        const int = try readTBigEndian(i64, reader);
        return int;
    }

    fn parseAssumeDouble(
        self: *Self,
        reader: anytype,
    ) !f64 {
        _ = self;
        const int = try readTBigEndian(f64, reader);
        return int;
    }

    fn parseAssumeCompound(
        self: *Self,
        reader: anytype,
    ) anyerror!AstStore.NodeIndex {
        var namedTags = std.ArrayList(AstStore.NamedTag).init(self.allocator);
        defer namedTags.deinit();

        var namedTag = try self.parseNamedTag(reader);
        while (!self.store.isEndTag(namedTag.idx)) : (namedTag = try parseNamedTag(self, reader)) {
            try namedTags.append(namedTag);
        }

        return self.store.newCompoundFromTags(namedTags.items);
    }

    pub fn deinit(self: *Self) void {
        self.arena.deinit();
        self.arena.child_allocator.destroy(self.arena);
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

test "Servers.dat" {
    const allocator = std.testing.allocator;
    const servers = @embedFile("./testFiles/servers.dat");
    var arenaAlloc = std.heap.ArenaAllocator.init(allocator);
    defer arenaAlloc.deinit();
    var parser = try Parser.init(allocator);
    defer parser.deinit();
    const nt = try parser.parseFromBytes(
        servers,
    );
    _ = nt;
}

test "GTNH.dat" {
    const allocator = std.testing.allocator;
    const servers = @embedFile("./testFiles/GTNH_level.dat");
    var arenaAlloc = std.heap.ArenaAllocator.init(allocator);
    defer arenaAlloc.deinit();

    var stream = std.io.fixedBufferStream(servers);

    var decompressedData = std.ArrayList(u8).init(allocator);
    defer decompressedData.deinit();
    try std.compress.gzip.decompress(stream.reader(), decompressedData.writer());
    var parser = try Parser.init(allocator);
    defer parser.deinit();
    const nt = try parser.parseFromBytes(
        decompressedData.items,
    );
    _ = nt;
}
