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

pub const AstStore = struct {
    allocator: std.mem.Allocator,

    tag_store: std.ArrayList(TAG),
    idx_store: std.ArrayList(NodeIndex),

    byte_store: std.ArrayList(i8),

    short_store: std.ArrayList(i16),

    int_store: std.ArrayList(i32),

    long_store: std.ArrayList(i64),

    float_store: std.ArrayList(f32),

    double_store: std.ArrayList(f64),

    byte_array: std.ArrayList(u8),
    byte_array_idx: std.ArrayList(NodeIndex),
    byte_array_len: std.ArrayList(u16),

    list_idx: std.ArrayList(NodeIndex),
    list_len: std.ArrayList(u16),

    compound_idx: std.ArrayList(NodeIndex),
    compount_len: std.ArrayList(u16),

    name_idx: std.ArrayList(NodeIndex),
    name_len: std.ArrayList(u16),

    compound_name: std.ArrayList(NodeIndex),
    compount_node: std.ArrayList(NodeIndex),

    pub const NodeIndex = u16;

    const Self = @This();

    pub fn init(allocator: std.mem.Allocator) Self {
        return Self{
            .allocator = allocator,
            .tag_store = .init(allocator),
            .idx_store = .init(allocator),

            .byte_store = .init(allocator),

            .short_store = .init(allocator),

            .int_store = .init(allocator),

            .long_store = .init(allocator),

            .float_store = .init(allocator),

            .double_store = .init(allocator),

            .byte_array = .init(allocator),
            .byte_array_idx = .init(allocator),
            .byte_array_len = .init(allocator),

            .list_idx = .init(allocator),
            .list_len = .init(allocator),

            .compound_idx = .init(allocator),
            .compount_len = .init(allocator),

            .name_idx = .init(allocator),
            .name_len = .init(allocator),

            .compound_name = .init(allocator),
            .compount_node = .init(allocator),
        };
    }

    pub fn newNode(self: *Self, tag: TAG, idx: NodeIndex) !NodeIndex {
        try self.tag_store.append(tag);
        const out_idx = self.tag_store.items.len - 1;
        try self.idx_store.append(idx);

        std.debug.assert(self.tag_store.items.len == self.idx_store.items.len);

        return @intCast(out_idx);
    }

    pub fn newByte(self: *Self, value: i8) !NodeIndex {
        const idx = try self.addItem(.byte_store, value);

        return self.newNode(.Byte, idx);
    }

    test newByte {
        var store = setupTest();
        defer store.deinit();

        const idx = try store.newByte(1);

        try std.testing.expectEqual(TAG.Byte, store.tag_store.items[idx]);
        try std.testing.expectEqual(1, store.byte_store.items[store.idx_store.items[idx]]);
    }

    pub fn newShort(self: *Self, value: i16) !NodeIndex {
        const idx = try self.addItem(.short_store, value);

        return self.newNode(.Short, idx);
    }

    test newShort {
        var store = setupTest();
        defer store.deinit();

        const idx = try store.newShort(1);

        try std.testing.expectEqual(TAG.Short, store.tag_store.items[idx]);
        try std.testing.expectEqual(1, store.short_store.items[store.idx_store.items[idx]]);
    }

    pub fn newInt(self: *Self, value: i32) !NodeIndex {
        const idx = try self.addItem(.int_store, value);

        return self.newNode(.Int, idx);
    }

    test newInt {
        var store = setupTest();
        defer store.deinit();

        const idx = try store.newInt(1);

        try std.testing.expectEqual(TAG.Int, store.tag_store.items[idx]);
        try std.testing.expectEqual(1, store.int_store.items[store.idx_store.items[idx]]);
    }

    pub fn newLong(self: *Self, value: i64) !NodeIndex {
        const idx = try self.addItem(.long_store, value);
        return self.newNode(.Long, idx);
    }

    test newLong {
        var store = setupTest();
        defer store.deinit();

        const idx = try store.newLong(1);

        try std.testing.expectEqual(TAG.Long, store.tag_store.items[idx]);
        try std.testing.expectEqual(1, store.long_store.items[store.idx_store.items[idx]]);
    }

    pub fn newFloat(self: *Self, value: f32) !NodeIndex {
        const idx = try self.addItem(.float_store, value);
        return self.newNode(.Float, idx);
    }

    test newFloat {
        const allocator = std.testing.allocator;
        var store = Self.init(allocator);
        defer store.deinit();

        const idx = try store.newFloat(1.0);

        try std.testing.expectEqual(TAG.Float, store.tag_store.items[idx]);
        try std.testing.expectEqual(1.0, store.float_store.items[store.idx_store.items[idx]]);
    }

    fn setupTest() Self {
        const allocator = std.testing.allocator;
        return Self.init(allocator);
    }

    pub fn addItem(
        self: *Self,
        comptime field: std.meta.FieldEnum(Self),
        value: FieldType(field),
    ) !NodeIndex {
        try @field(self, @tagName(field)).append(value);
        return @intCast(@field(self, @tagName(field)).items.len - 1);
    }

    pub fn newDouble(self: *Self, value: f64) !NodeIndex {
        const idx = try self.addItem(.double_store, value);
        return self.newNode(.Double, idx);
    }

    test newDouble {
        const allocator = std.testing.allocator;
        var store = Self.init(allocator);
        defer store.deinit();

        const idx = try store.newDouble(1.0);

        try std.testing.expectEqual(TAG.Double, store.tag_store.items[idx]);
        try std.testing.expectEqual(1.0, store.double_store.items[store.idx_store.items[idx]]);
    }

    fn FieldType(field: std.meta.FieldEnum(Self)) type {
        const arrListType: type = @FieldType(Self, @tagName(field));
        const items: type = @FieldType(arrListType, "items");
        const itemsInfo = @typeInfo(items);
        return itemsInfo.pointer.child;
    }

    pub fn newByteArray(self: *Self, arr: []const u8) !NodeIndex {
        const start = self.byte_array.items.len;
        try self.byte_array.appendSlice(arr);
        const end = self.byte_array.items.len;
        std.debug.assert(end - start == arr.len);
        const idx = try self.addItem(.byte_array_idx, @intCast(start));
        const lenIdx = try self.addItem(.byte_array_len, @intCast(arr.len));
        std.debug.assert(idx == lenIdx);

        return self.newNode(.ByteArray, idx);
    }

    test newByteArray {
        var store = setupTest();
        defer store.deinit();

        const arr = [_]u8{ 1, 2, 3, 4, 5 };

        const idx = try store.newByteArray(arr[0..]);

        try std.testing.expectEqual(TAG.ByteArray, store.tag_store.items[idx]);
        const arrIdx = store.byte_array_idx.items[idx];
        try std.testing.expectEqual(0, arrIdx);
        const arrLen = store.byte_array_len.items[idx];
        try std.testing.expectEqual(arr.len, arrLen);

        const gotArr = store.byte_array.items[arrIdx..arrLen];

        try std.testing.expectEqualSlices(u8, &arr, gotArr);
    }

    pub fn newString(self: *Self, str: []const u8) !NodeIndex {
        const start = self.byte_array.items.len;
        try self.byte_array.appendSlice(str);
        const end = self.byte_array.items.len;
        std.debug.assert(end - start == str.len);
        const idx = try self.addItem(.byte_array_idx, @intCast(start));
        const lenIdx = try self.addItem(.byte_array_len, @intCast(str.len));
        std.debug.assert(idx == lenIdx);

        return self.newNode(.String, idx);
    }

    test newString {
        var store = setupTest();
        defer store.deinit();

        const arr = "Hello World";

        const idx = try store.newByteArray(arr[0..]);

        try std.testing.expectEqual(TAG.ByteArray, store.tag_store.items[idx]);
        const arrIdx = store.byte_array_idx.items[idx];
        try std.testing.expectEqual(0, arrIdx);
        const arrLen = store.byte_array_len.items[idx];
        try std.testing.expectEqual(arr.len, arrLen);

        const gotArr = store.byte_array.items[arrIdx..arrLen];

        try std.testing.expectEqualSlices(u8, arr, gotArr);
    }

    pub fn deinit(self: *Self) void {
        self.tag_store.deinit();
        self.idx_store.deinit();

        self.byte_store.deinit();

        self.short_store.deinit();

        self.int_store.deinit();

        self.long_store.deinit();

        self.float_store.deinit();

        self.double_store.deinit();

        self.byte_array.deinit();
        self.byte_array_idx.deinit();
        self.byte_array_len.deinit();

        self.list_idx.deinit();
        self.list_len.deinit();

        self.compound_idx.deinit();
        self.compount_len.deinit();

        self.name_idx.deinit();
        self.name_len.deinit();

        self.compound_name.deinit();
        self.compount_node.deinit();
    }
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
