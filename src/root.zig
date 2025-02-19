//! By convention, root.zig is the root source file when making a library. If
//! you are making an executable, the convention is to delete this file and
//! start with main.zig instead.
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
};

pub const Node = union(TAG) {
    const Self = @This();
    End: null,
    Byte: i8,
    Short: i16,
    Int: i32,
    Long: i64,
    Float: f32,
    Double: f64,
    ByteArray: []u8,
    String: []u8,
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
        writer.print("TAG_{s}: ", .{@tagName(self)});
        switch (self) {
            .End => return,
            .Byte => |b| writer.print(
                "{}",
                .{b},
            ),
            .Short => |s| writer.print(
                "{}",
                .{s},
            ),
            .Int => |i| writer.print(
                "{}",
                .{i},
            ),
            .Long => |l| writer.print(
                "{}",
                .{l},
            ),
            .Float => |f| writer.print(
                "{}",
                .{f},
            ),
            .Double => |d| writer.print(
                "{}",
                .{d},
            ),
            .ByteArray => |ba| writer.print(
                "{any}",
                .{ba},
            ),
            .String => |s| writer.print(
                "{s}",
                .{s},
            ),
            .List => |s| writer.print(
                "{s}",
                .{s},
            ),
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
        if (self.tag != .End) {
            writer.print("TAG_{s}(\"{s}\"): ", .{ @tagName(self.tag), self.name });
        }
        switch (self.tag) {
            .End => return,
            .Byte => |b| writer.print(
                "{}",
                .{b},
            ),
            .Short => |s| writer.print(
                "{}",
                .{s},
            ),
            .Int => |i| writer.print(
                "{}",
                .{i},
            ),
            .Long => |l| writer.print(
                "{}",
                .{l},
            ),
            .Float => |f| writer.print(
                "{}",
                .{f},
            ),
            .Double => |d| writer.print(
                "{}",
                .{d},
            ),
            .ByteArray => |ba| writer.print(
                "{any}",
                .{ba},
            ),
            .String => |s| writer.print(
                "{s}",
                .{s},
            ),
            .List => |l| {
                writer.print("{} entries of type TAG_{s}\n{{\n", .{ l.items.len, @tagName(l.tag) });
                for (l.items) |value| {
                    value.format(fmt, options, writer);
                    writer.print("\n", .{});
                }
                writer.print("}}\n", .{});
            },
            .Compound => |c| {
                writer.print("{} entries\n{\n", .{c.items.len});
                for (c.items) |value| {
                    value.format(fmt, options, writer);
                    writer.print("\n", .{});
                }
                writer.print("}}\n", .{});
            },
        }
    }
};
