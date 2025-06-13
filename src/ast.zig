const std = @import("std");
const testing = std.testing;

const utils = @import("utils.zig");

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
    ///          An array of bytes of unspecified format. The length of this
    ///          array is <length> bytes
    ByteArray = 7,

    /// TYPE: 8  NAME: TAG_String
    /// Payload: TAG_Short length
    ///          An array of bytes defining a string in UTF-8 format. The
    ///          length of this array is <length> bytes
    String = 8,

    /// TYPE: 9  NAME: TAG_List
    /// Payload: TAG_Byte tagId
    ///          TAG_Int length
    ///          A sequential list of Tags (not Named Tags), of type <typeId>.
    ///          The length of this array is <length> Tags
    /// Notes:   All tags share the same type.
    List = 9,

    /// TYPE: 10 NAME: TAG_Compound
    /// Payload: A sequential list of Named Tags. This array keeps going until
    ///          a TAG_End is found.
    ///          TAG_End end
    /// Notes:   If there's a nested TAG_Compound within this tag, that one
    ///          will also have a TAG_End, so simply reading until the next
    ///          TAG_End will not work.
    ///          The names of the named tags have to be unique within each
    ///          TAG_Compound
    ///          The order of the tags is not guaranteed.
    Compound = 10,

    /// TYPE: 11 NAME: TAG_Int_Array
    /// Payload: TAG_Int length
    ///          An array of ints of unspecified format. The length of this
    ///          array is <length> bytes
    IntArray = 11,

    /// TYPE: 12 NAME: TAG_Long_Array
    /// Payload: TAG_Int length
    ///          An array of ints of unspecified format. The length of this
    ///          array is <length> bytes
    LongArray = 12,

    pub fn typeOf(tag: TAG) type {
        return switch (tag) {
            .End => void,
            .Byte => i8,
            .Short => i16,
            .Int => i32,
            .Long => i64,
            .Float => f32,
            .Double => f64,
            .ByteArray => []u8,
            .String => []u8,
            else => @compileError("Unsupported tag: " ++ @tagName(tag)),
        };
    }

    const Self = @This();

    pub fn ofType(comptime T: type) Self {
        const tInfo: std.builtin.Type = @typeInfo(T);

        switch (tInfo) {
            .int => |int| {
                if (int.signedness == .unsigned) {
                    @compileError("Does not support unsiged integers");
                }

                switch (int.bits) {
                    8 => return TAG.Byte,
                    16 => return TAG.Short,
                    32 => return TAG.Int,
                    64 => return TAG.Long,
                    else => {
                        @compileError(std.fmt.comptimePrint(
                            "Does not support intagers of size: ",
                            .{int.bits},
                        ));
                    },
                }
            },
            .float => |float| {
                switch (float.bits) {
                    32 => return TAG.Float,
                    64 => return TAG.Double,
                    else => {
                        @compileError(std.fmt.comptimePrint(
                            "Does not support float of size: ",
                            .{float.bits},
                        ));
                    },
                }
            },
            .pointer => |ptr| {
                switch (ptr.size) {
                    .one => return ofType(ptr.child),
                    .c => @compileError("Does not support c pointers"),
                    else => {},
                }

                if (specializePtr(ptr.child)) |special| {
                    return special;
                }

                return TAG.List;
            },
            else => {
                @compileError("Unsupported type: " ++ @tagName(tInfo));
            },
        }
    }

    fn specializePtr(comptime T: type) ?TAG {
        const tInfo = @typeInfo(T);

        switch (tInfo) {
            .int => |int| {
                switch (int.bits) {
                    8 => {
                        if (int.signedness == .unsigned) {
                            return TAG.ByteArray;
                        }
                    },
                    32 => return TAG.IntArray,
                    64 => return TAG.LongArray,
                    else => {},
                }
            },
            else => {},
        }

        return null;
    }
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
    byte_array_len: std.ArrayList(NodeLen),

    list_idx: std.ArrayList(NodeIndex),
    list_len: std.ArrayList(NodeLen),

    compound_idx: std.ArrayList(NodeIndex),
    compound_len: std.ArrayList(NodeLen),

    name_idx: std.ArrayList(NodeIndex),
    name_len: std.ArrayList(NodeLen),

    compound_name: std.ArrayList(NodeIndex),
    compound_node: std.ArrayList(NodeIndex),

    int_array_idx: std.ArrayList(NodeIndex),
    int_array_len: std.ArrayList(NodeLen),

    long_array_idx: std.ArrayList(NodeIndex),
    long_array_len: std.ArrayList(NodeLen),

    pub const NodeIndex = u16;
    pub const NodeLen = u16;

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
            .compound_len = .init(allocator),

            .name_idx = .init(allocator),
            .name_len = .init(allocator),

            .compound_name = .init(allocator),
            .compound_node = .init(allocator),

            .int_array_idx = .init(allocator),
            .int_array_len = .init(allocator),

            .long_array_idx = .init(allocator),
            .long_array_len = .init(allocator),
        };
    }

    pub fn newNode(self: *Self, tag: TAG, idx: NodeIndex) !NodeIndex {
        try self.tag_store.append(tag);
        try self.idx_store.append(idx);

        std.debug.assert(self.tag_store.items.len == self.idx_store.items.len);
        const out_idx = self.tag_store.items.len - 1;

        return @intCast(out_idx);
    }

    pub fn newByte(self: *Self, value: i8) !NodeIndex {
        const idx = try self.addItem(.Byte, value);

        return self.newNode(.Byte, idx);
    }

    test newByte {
        var store = setupTest();
        defer store.deinit();

        const idx = try store.newByte(1);

        try std.testing.expectEqual(TAG.Byte, store.tag_store.items[idx]);
        const byte_idx = store.idx_store.items[idx];
        try std.testing.expectEqual(1, store.byte_store.items[byte_idx]);
    }

    pub fn newShort(self: *Self, value: i16) !NodeIndex {
        const idx = try self.addItem(.Short, value);

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
        const idx = try self.addItem(.Int, value);

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
        const idx = try self.addItem(.Long, value);
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
        const idx = try self.addItem(.Float, value);
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
        comptime tag: TAG,
        value: anytype,
    ) !NodeIndex {
        return addItemToStore(self.getStore(tag), value);
    }

    fn addItemToStore(store: anytype, value: anytype) !NodeIndex {
        if (StoreType(@TypeOf(store)) != @TypeOf(value)) {
            @compileError(std.fmt.comptimePrint(
                \\ value is wrong type 
                \\ expected: {}
                \\ got: {}
            ,
                .{
                    StoreType(@TypeOf(store)),
                    @TypeOf(value),
                },
            ));
        }
        try store.append(value);
        return @intCast(store.items.len - 1);
    }

    pub fn newDouble(self: *Self, value: f64) !NodeIndex {
        const idx = try self.addItem(.Double, value);
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
        return StoreType(arrListType);
    }

    fn StoreType(T: type) type {
        const storeType = switch (@typeInfo(T)) {
            .@"struct" => T,
            .pointer => |ptr| ptr.child,
            else => @compileError("Unsupported type: " ++ @typeName(T)),
        };
        const items: type = @FieldType(storeType, "items");
        const itemsInfo = @typeInfo(items);
        return itemsInfo.pointer.child;
    }

    fn toIdx(int: anytype) NodeIndex {
        const inputT: type = @TypeOf(int);
        const tInfo = @typeInfo(inputT);

        if (tInfo != .int) {
            @compileError("int must be an integer");
        }

        return @intCast(int);
    }

    fn toLen(int: anytype) NodeLen {
        const inputT: type = @TypeOf(int);
        const tInfo = @typeInfo(inputT);

        if (tInfo != .int) {
            @compileError("int must be an integer");
        }

        return @intCast(int);
    }

    fn addToByteArray(self: *Self, arr: []const u8) !NodeIndex {
        const start = self.byte_array.items.len;
        try self.byte_array.appendSlice(arr);
        const end = self.byte_array.items.len;
        std.debug.assert(end - start == arr.len);
        return toIdx(start);
    }

    fn newByteArrayInner(self: *Self, arr: []const u8) !NodeIndex {
        const start = try self.addToByteArray(arr);
        const idx = try addItemToStore(&self.byte_array_idx, start);
        const lenIdx = try addItemToStore(&self.byte_array_len, toLen(arr.len));
        std.debug.assert(idx == lenIdx);
        return idx;
    }

    pub fn newByteArray(self: *Self, arr: []const u8) !NodeIndex {
        const idx = try self.newByteArrayInner(arr);
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
        const idx = try self.newByteArrayInner(str);

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

    fn debugPrint(self: *const Self) void {
        std.debug.print("{s} {{\n", .{@typeName(Self)});

        inline for (std.meta.fields(Self)) |field| {
            if (comptime std.mem.eql(u8, field.name, "allocator")) {
                continue;
            }

            std.debug.print("  {s}: {any}\n", .{ field.name, @field(self, field.name).items });
        }

        std.debug.print("}}", .{});
    }

    pub fn newList(self: *Self, comptime tag: TAG, len: u16) !struct {
        idx: NodeIndex,
        arr: []NodeIndex,
    } {
        std.debug.assert(self.tag_store.items.len == self.idx_store.items.len);
        const start_idx = self.idx_store.items.len;
        const arr = try self.idx_store.addManyAsSlice(@intCast(len));
        errdefer {
            self.idx_store.items = self.idx_store.items[0..start_idx];
        }
        try self.tag_store.appendNTimes(tag, len);

        const list_idx = try addItemToStore(&self.list_idx, toIdx(start_idx));
        try self.list_len.append(len);

        const idx = try self.newNode(.List, list_idx);

        utils.assertEq(idx + 1, @as(u16, @intCast(self.idx_store.items.len)));

        return .{
            .idx = idx,
            .arr = arr,
        };
    }

    test newList {
        const input = [_]comptime_int{ 1, 2, 3, 4, 5 };

        inline for ([_]TAG{ .Byte, .Short, .Int, .Float, .Double }) |tag| {
            try testList(tag, &input);
        }

        {
            const inner = createTest(u8, input);
            const byteArr: [8][]const u8 = @splat(inner[0..]);

            try testByteArray(&byteArr);
        }

        {
            const strArr: [8][]const u8 = @splat("Hello");
            try testByteArray(&strArr);
        }

        {
            const inner = createTest(i8, input);
            const listArr: [8][]const i8 = @splat(inner[0..]);

            var store = Self.setupTest();
            defer store.deinit();

            const list = try store.newList(.List, toLen(listArr.len));
            for (listArr, 0..) |item, i| {
                const innerArr = try store.newList(.Byte, toLen(item.len));
                for (item, 0..) |innerItem, j| {
                    innerArr.arr[j] = try store.addItem(.Byte, innerItem);
                }
                list.arr[i] = innerArr.idx;
            }

            try std.testing.expectEqual(.List, store.tag_store.items[list.idx]);

            const idx = store.idx_store.items[list.idx];
            const list_idx = store.list_idx.items[idx];
            const list_len = store.list_len.items[idx];
            const list_arr = store.idx_store.items[list_idx .. list_idx + list_len];

            for (listArr, 0..) |item, i| {
                const val_idx = list_arr[i];
                try std.testing.expectEqual(.List, store.tag_store.items[idx + i]);

                const item_idx = store.idx_store.items[val_idx];
                const inner_idx = store.list_idx.items[item_idx];
                const inner_len = store.list_len.items[item_idx];
                const inner_arr = store.idx_store.items[inner_idx .. inner_idx + inner_len];

                for (item, 0..) |innerItem, j| {
                    const inner_val_idx = inner_arr[j];
                    try std.testing.expectEqual(.Byte, store.tag_store.items[inner_idx + j]);
                    const val = store.byte_store.items[inner_val_idx];

                    try std.testing.expectEqual(innerItem, val);
                }
            }
        }
    }

    fn createTest(comptime T: type, input: anytype) [input.len]T {
        var out: [input.len]T = undefined;

        inline for (0..input.len) |i| {
            out[i] = input[i];
        }

        return out;
    }

    fn testList(
        comptime tag: TAG,
        input: []const comptime_int,
    ) !void {
        var store = Self.setupTest();
        defer store.deinit();

        const list = try store.newList(tag, input.len);
        inline for (input, 0..) |item, i| {
            list.arr[i] = try store.addItem(tag, @as(tag.typeOf(), item));
        }

        try std.testing.expectEqual(.List, store.tag_store.items[list.idx]);

        const idx = store.idx_store.items[list.idx];
        const list_idx = store.list_idx.items[idx];
        const list_len = store.list_len.items[idx];
        const list_arr = store.idx_store.items[list_idx .. list_idx + list_len];

        inline for (input, 0..) |item, i| {
            const val_idx = list_arr[i];
            try std.testing.expectEqual(tag, store.tag_store.items[i]);
            const val = store.getStore(tag).items[val_idx];

            try std.testing.expectEqual(item, val);
        }
    }

    fn testByteArray(byteArr: []const []const u8) !void {
        var store = Self.setupTest();
        defer store.deinit();

        const list = try store.newList(.ByteArray, toLen(byteArr.len));
        for (byteArr, 0..) |item, i| {
            list.arr[i] = try store.newByteArrayInner(item);
        }

        try std.testing.expectEqual(.List, store.tag_store.items[list.idx]);

        const idx = store.idx_store.items[list.idx];
        const list_idx = store.list_idx.items[idx];
        const list_len = store.list_len.items[idx];
        const list_arr = store.idx_store.items[list_idx .. list_idx + list_len];

        for (byteArr, 0..) |item, i| {
            const val_idx = list_arr[i];
            try std.testing.expectEqual(.ByteArray, store.tag_store.items[i]);
            const arr_idx = store.byte_array_idx.items[val_idx];
            const arr_len = store.byte_array_len.items[val_idx];
            const arr = store.byte_array.items[arr_idx .. arr_idx + arr_len];

            try std.testing.expectEqualSlices(u8, item, arr);
        }
    }

    fn convertTestType(comptime T: type) type {
        const tInfo: std.builtin.Type = @typeInfo(T);

        switch (tInfo) {
            .int, .float => return T,
            .pointer => |ptr| {
                var out = ptr;
                out.is_const = true;

                return @Type(std.builtin.Type{
                    .pointer = out,
                });
            },
            else => @compileError("Unsupported type: " ++ @tagName(tInfo)),
        }
    }

    pub const CompoundParam = struct {
        name: []const u8,
        idx: NodeIndex,
    };

    pub fn newCompound(self: *Self, params: []const CompoundParam) !NodeIndex {
        std.debug.assert(self.tag_store.items.len == self.idx_store.items.len);
        std.debug.assert(self.compound_name.items.len == self.compound_node.items.len);
        const start_idx = self.compound_name.items.len;
        const name_arr = try self.compound_name.addManyAsSlice(params.len);
        const node_arr = try self.compound_node.addManyAsSlice(params.len);

        const name_start_idx = self.name_idx.items.len;
        const name_idxs = try self.name_idx.addManyAsSlice(params.len);
        const name_lens = try self.name_len.addManyAsSlice(params.len);

        for (params, 0..) |param, i| {
            name_idxs[i] = try self.addToByteArray(param.name);
            name_lens[i] = toLen(param.name.len);

            name_arr[i] = toIdx(name_start_idx + i);
            node_arr[i] = param.idx;
        }

        const idx = self.compound_idx.items.len;
        try self.compound_idx.append(toIdx(start_idx));
        try self.compound_len.append(toLen(params.len));

        return self.newNode(.Compound, toIdx(idx));
    }

    test newCompound {
        var store = setupTest();
        defer store.deinit();

        const name = try store.newString("Ethan");
        const age = try store.newByte(22);
        const rating = try store.newFloat(0.9);

        const params = [_]CompoundParam{
            CompoundParam{
                .name = "name",
                .idx = name,
            },
            CompoundParam{
                .name = "age",
                .idx = age,
            },
            CompoundParam{
                .name = "rating",
                .idx = rating,
            },
        };

        const idx = try store.newCompound(&params);

        try std.testing.expectEqual(TAG.Compound, store.tag_store.items[idx]);
        const compound_store_idx = store.idx_store.items[idx];
        const compound_idx = store.compound_idx.items[compound_store_idx];
        const compound_len = store.compound_len.items[compound_store_idx];
        try std.testing.expectEqual(params.len, compound_len);

        const name_arr = store.compound_name.items[compound_idx .. compound_idx + compound_len];
        const node_arr = store.compound_node.items[compound_idx .. compound_idx + compound_len];

        for (params, 0..) |param, i| {
            const name_idx = name_arr[i];
            const name_byte_idx = store.name_idx.items[name_idx];
            const name_byte_len = store.name_len.items[name_idx];

            const param_name = store.byte_array.items[name_byte_idx .. name_byte_idx + name_byte_len];

            try std.testing.expectEqualStrings(param.name, param_name);

            try std.testing.expectEqual(param.idx, node_arr[i]);
        }
    }

    fn newIntArrayInner(self: *Self, arr: []const i32) !NodeIndex {
        const start = self.int_store.items.len;
        try self.int_store.appendSlice(arr);
        const end = self.int_store.items.len;
        std.debug.assert(end - start == arr.len);

        const idx = try addItemToStore(&self.int_array_idx, toIdx(start));
        const lenIdx = try addItemToStore(&self.int_array_len, toLen(arr.len));
        std.debug.assert(idx == lenIdx);
        return idx;
    }

    fn newIntArray(self: *Self, arr: []const i32) !NodeIndex {
        const idx = try self.newIntArrayInner(arr);
        return self.newNode(.IntArray, idx);
    }

    test newIntArray {
        var store = setupTest();
        defer store.deinit();

        const arr = [_]i32{ 1, 2, 3, 4, 5 };

        const idx = try store.newIntArray(arr[0..]);

        try std.testing.expectEqual(TAG.IntArray, store.tag_store.items[idx]);
        const arrIdx = store.int_array_idx.items[idx];
        try std.testing.expectEqual(0, arrIdx);
        const arrLen = store.int_array_len.items[idx];
        try std.testing.expectEqual(arr.len, arrLen);

        const gotArr = store.int_store.items[arrIdx..arrLen];

        try std.testing.expectEqualSlices(i32, &arr, gotArr);
    }

    fn newLongArrayInner(self: *Self, arr: []const i64) !NodeIndex {
        const start = self.long_store.items.len;
        try self.long_store.appendSlice(arr);
        const end = self.long_store.items.len;
        std.debug.assert(end - start == arr.len);

        const idx = try addItemToStore(&self.long_array_idx, toIdx(start));
        const lenIdx = try addItemToStore(&self.long_array_len, toLen(arr.len));
        std.debug.assert(idx == lenIdx);
        return idx;
    }

    fn newLongArray(self: *Self, arr: []const i64) !NodeIndex {
        const idx = try self.newLongArrayInner(arr);
        return self.newNode(.LongArray, idx);
    }

    test newLongArray {
        var store = setupTest();
        defer store.deinit();

        const arr = [_]i64{ 1, 2, 3, 4, 5 };

        const idx = try store.newLongArray(arr[0..]);

        try std.testing.expectEqual(TAG.LongArray, store.tag_store.items[idx]);
        const arrIdx = store.long_array_idx.items[idx];
        try std.testing.expectEqual(0, arrIdx);
        const arrLen = store.long_array_len.items[idx];
        try std.testing.expectEqual(arr.len, arrLen);

        const gotArr = store.long_store.items[arrIdx..arrLen];

        try std.testing.expectEqualSlices(i64, &arr, gotArr);
    }

    fn getStore(self: *Self, comptime tag: TAG) *std.ArrayList(tag.typeOf()) {
        switch (tag) {
            .End => @compileError("Not Supported"),
            .Byte => return &self.byte_store,
            .Short => return &self.short_store,
            .Int => return &self.int_store,
            .Long => return &self.long_store,
            .Float => return &self.float_store,
            .Double => return &self.double_store,
            else => @compileError("Tag not supported: " ++ @tagName(tag)),
        }
    }

    pub fn getTag(self: *const Self, idx: NodeIndex) TAG {
        return self.tag_store.items[idx];
    }

    fn expectEqualSingle(
        self: *Self,
        comptime tag: TAG,
        expected: NodeIndex,
        actual: NodeIndex,
    ) !void {
        const store = self.getStore(tag);
        const expectedByte = store.items[expected];
        const actualByte = store.items[actual];

        try std.testing.expectEqual(expectedByte, actualByte);
    }

    fn getByteArray(self: *const Self, idx: NodeIndex) []const u8 {
        const arr_idx = self.byte_array_idx.items[idx];
        const arr_len = self.byte_array_len.items[idx];

        return self.byte_array.items[arr_idx .. arr_idx + arr_len];
    }

    fn expectNodeEqual(
        self: *Self,
        tag: TAG,
        expected: NodeIndex,
        actual: NodeIndex,
    ) !void {
        switch (tag) {
            TAG.Byte => {
                return self.expectEqualSingle(.Byte, expected, actual);
            },
            TAG.Short => {
                return self.expectEqualSingle(.Short, expected, actual);
            },
            TAG.Int => {
                return self.expectEqualSingle(.Int, expected, actual);
            },
            TAG.Long => {
                return self.expectEqualSingle(.Long, expected, actual);
            },
            TAG.Float => {
                return self.expectEqualSingle(.Float, expected, actual);
            },
            TAG.Double => {
                return self.expectEqualSingle(.Double, expected, actual);
            },
            TAG.ByteArray => {
                const expected_arr = self.getByteArray(expected);
                const actual_arr = self.getByteArray(actual);

                try std.testing.expectEqualSlices(u8, expected_arr, actual_arr);
            },
            TAG.String => {
                const expected_arr = self.getByteArray(expected);
                const actual_arr = self.getByteArray(actual);

                try std.testing.expectEqualStrings(expected_arr, actual_arr);
            },
            else => {
                unreachable;
            },
        }
    }

    pub fn expectEqualDeep(self: *Self, expected: NodeIndex, actual: NodeIndex) !void {
        if (expected == actual) {
            return;
        }

        const expectedTag = self.getTag(expected);
        const actualTag = self.getTag(actual);

        if (expectedTag != actualTag) {
            std.debug.print(
                \\ Missmatch of tags
                \\ Expected tag: {}
                \\ Got tag: {}
            , .{
                expectedTag,
                actualTag,
            });
            return error.TagMissmatch;
        }

        const expectedIdx = self.idx_store.items[expected];
        const actualIdx = self.idx_store.items[actual];

        if (expectedIdx == actualIdx) {
            return;
        }

        return self.expectNodeEqual(expectedTag, expectedIdx, actualIdx);
    }

    fn checkInt(
        comptime tag: TAG,
        val: comptime_int,
        creator: fn (self: *Self, val: tag.typeOf()) anyerror!NodeIndex,
    ) !void {
        var store = setupTest();
        defer store.deinit();

        const initial = try creator(&store, val);
        try store.expectEqualDeep(initial, initial);

        const other = try creator(&store, val);
        try store.expectEqualDeep(initial, other);
    }

    test expectEqualDeep {
        inline for (.{
            .{ TAG.Byte, Self.newByte },
            .{ TAG.Short, Self.newShort },
            .{ TAG.Int, Self.newInt },
            .{ TAG.Long, Self.newLong },
            .{ TAG.Float, Self.newFloat },
            .{ TAG.Double, Self.newDouble },
        }) |value| {
            try checkInt(value[0], 1, value[1]);
        }

        {
            var store = setupTest();
            defer store.deinit();

            const arr = [_]u8{ 1, 2, 3, 4, 5 };

            const initial = try store.newByteArray(&arr);
            try store.expectEqualDeep(initial, initial);

            const other = try store.newByteArray(&arr);
            try store.expectEqualDeep(initial, other);
        }

        {
            var store = setupTest();
            defer store.deinit();

            const arr = "Hello World";

            const initial = try store.newString(arr);
            try store.expectEqualDeep(initial, initial);

            const other = try store.newString(arr);
            try store.expectEqualDeep(initial, other);
        }
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
        self.compound_len.deinit();

        self.name_idx.deinit();
        self.name_len.deinit();

        self.compound_name.deinit();
        self.compound_node.deinit();

        self.int_array_idx.deinit();
        self.int_array_len.deinit();

        self.long_array_idx.deinit();
        self.long_array_len.deinit();
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
