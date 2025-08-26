const std = @import("std");

const ast = @import("ast.zig");
const NamedTag = ast.NamedTag;
const Node = ast.Node;
const TAG = ast.TAG;

const parser = @import("parser.zig");

const parseFromBytes = parser.parseFromBytes;

pub fn Parsed(comptime T: type) type {
    return struct {
        arena: *std.heap.ArenaAllocator,
        value: T,

        pub fn deinit(self: @This()) void {
            const allocator = self.arena.child_allocator;
            self.arena.deinit();
            allocator.destroy(self.arena);
        }
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

pub fn deserialize(
    comptime T: type,
    allocator: std.mem.Allocator,
    reader: *std.io.Reader,
    options: ParseOptions,
) !Parsed(T) {
    var astArena = std.heap.ArenaAllocator.init(allocator);
    defer astArena.deinit();
    const rootNode = try parser.parseFromReader(reader, astArena.allocator());

    return deserializeNamedTagTree(T, allocator, rootNode, options);
}

pub fn deserializeNamedTagTree(
    comptime T: type,
    allocator: std.mem.Allocator,
    rootNode: NamedTag,
    options: ParseOptions,
) !Parsed(T) {
    var parsed = Parsed(T){
        .arena = try allocator.create(std.heap.ArenaAllocator),
        .value = undefined,
    };
    errdefer allocator.destroy(parsed.arena);
    parsed.arena.* = std.heap.ArenaAllocator.init(allocator);
    errdefer parsed.arena.deinit();

    parsed.value = try deserializeNode(T, parsed.arena.allocator(), rootNode.tag, options);

    return parsed;
}

test deserializeNamedTagTree {
    const allocator = std.testing.allocator;
    var fields = [_]NamedTag{
        .{
            .name = "byte",
            .tag = .{ .Byte = 1 },
        },
        .{
            .name = "short",
            .tag = .{ .Short = 2 },
        },
        .{
            .name = "int",
            .tag = .{ .Int = 3 },
        },
        .{
            .name = "long",
            .tag = .{ .Long = 4 },
        },
        .{
            .name = "float",
            .tag = .{ .Float = 5 },
        },
        .{
            .name = "double",
            .tag = .{ .Double = 6 },
        },
        .{
            .name = "byteArray",
            .tag = .{ .ByteArray = ([_]u8{ 1, 2, 3, 4, 5 })[0..] },
        },
        .{
            .name = "byteArrayAlloc",
            .tag = .{ .ByteArray = ([_]u8{ 1, 2, 3, 4, 5, 6 })[0..] },
        },
        .{
            .name = "string",
            .tag = .{ .String = "string" },
        },
        .{
            .name = "list",
            .tag = .{
                .List = .{
                    .tag = .Int,
                    .items = &[_]Node{
                        .{ .Int = 1 },
                        .{ .Int = 2 },
                        .{ .Int = 3 },
                        .{ .Int = 4 },
                    },
                },
            },
        },
        .{
            .name = "listAlloc",
            .tag = .{
                .List = .{
                    .tag = .Int,
                    .items = &[_]Node{
                        .{ .Int = 1 },
                        .{ .Int = 2 },
                        .{ .Int = 3 },
                        .{ .Int = 4 },
                    },
                },
            },
        },
        .{
            .name = "compound",
            .tag = .{
                .Compound = &.{
                    .{
                        .name = "a",
                        .tag = .{
                            .Int = 1,
                        },
                    },
                    .{
                        .name = "b",
                        .tag = .{
                            .Int = 2,
                        },
                    },
                    .{
                        .name = "c",
                        .tag = .{
                            .Int = 3,
                        },
                    },
                },
            },
        },
        .{
            .name = "optionalValue",
            .tag = .{ .String = "string" },
        },
    };
    const input = NamedTag{ .name = "", .tag = .{
        .Compound = &fields,
    } };

    const outT = struct {
        byte: i8 = 1,
        short: i16 = 2,
        int: i32 = 3,
        long: i64 = 4,
        float: f32 = 5,
        double: f64 = 6,
        byteArray: [5]u8 = [_]u8{ 1, 2, 3, 4, 5 },
        byteArrayAlloc: []const u8 = ([_]u8{ 1, 2, 3, 4, 5, 6 })[0..],
        string: []const u8 = "string",
        list: [4]i32 = [_]i32{ 1, 2, 3, 4 },
        listAlloc: []const i32 = ([_]i32{ 1, 2, 3, 4 })[0..],
        compound: struct {
            a: i32 = 1,
            b: i32 = 2,
            c: i32 = 3,
        } = .{},
        optional: ?i32 = null,
        defaultValue: i32 = 10,
        optionalValue: ?[]const u8 = "string",
    };

    const expected = outT{};

    const out = try deserializeNamedTagTree(outT, allocator, input, .{
        .printErrors = true,
    });
    defer out.deinit();

    try std.testing.expectEqualDeep(expected, out.value);
}

fn deserializeNamedTagInner(
    comptime T: type,
    allocator: std.mem.Allocator,
    rootNode: NamedTag,
    options: ParseOptions,
) !struct { name: []const u8, value: T } {
    const name = try allocator.dupe(u8, rootNode.name);
    errdefer allocator.free(rootNode.name);

    const value = try deserializeNode(T, allocator, rootNode.tag, options);
    return .{
        .name = name,
        .value = value,
    };
}

fn deserializeNode(
    comptime T: type,
    allocator: std.mem.Allocator,
    node: Node,
    options: ParseOptions,
) !T {
    const tInfo = @typeInfo(T);

    switch (tInfo) {
        .int => |i| {
            return deserializeInt(i, allocator, node, options);
        },
        .@"struct" => {
            return deserializeStruct(T, allocator, node, options);
        },
        .float => |f| {
            return deserializeFloat(f, allocator, node, options);
        },
        .array => {
            return deserializeArray(T, allocator, node, options);
        },
        .pointer => |pt| {
            return deserializePointer(pt, allocator, node, options);
        },
        .optional => |op| {
            return try deserializeNode(op.child, allocator, node, options);
        },
        else => @compileError("Unsupported base type: " ++ @tagName(tInfo)),
    }
}

const Type = std.builtin.Type;

fn deserializePointer(
    comptime pT: Type.Pointer,
    allocator: std.mem.Allocator,
    node: Node,
    options: ParseOptions,
) !@Type(Type{ .pointer = pT }) {
    switch (pT.size) {
        .C => {
            @compileError("C pointers are not supported");
        },
        .One => {
            const ptr = try allocator.create(pT.child);
            errdefer allocator.destroy(ptr);
            ptr.* = try deserializeNode(pT.child, allocator, node, options);
            return ptr;
        },
        .Slice => {
            switch (node) {
                .ByteArray => |ba| {
                    if (pT.child != u8) {
                        options.logErr("Cannot deserialize TAG_ByteArray into: []{s}", .{
                            @typeName(pT.child),
                        });
                        return error.NotByteArray;
                    }
                    return allocator.dupe(u8, ba);
                },
                .String => |str| {
                    if (pT.child != u8) {
                        options.logErr("Cannot deserialize TAG_String into: []{s}", .{
                            @typeName(pT.child),
                        });
                        return error.NotByteArray;
                    }
                    return allocator.dupe(u8, str);
                },
                .List => |l| {
                    if (pT.child == u8) {
                        unreachable;
                    }
                    var out = try allocator.alloc(pT.child, l.items.len);
                    errdefer allocator.free(out);
                    for (0..out.len) |i| {
                        out[i] = try deserializeNode(pT.child, allocator, l.items[i], options);
                    }
                    return out;
                },
                else => {
                    options.logErr(
                        "It is not supported to deserialize a TAG_{s} into a slice",
                        .{
                            @tagName(node),
                        },
                    );
                    return error.UnsupportedTagForSlice;
                },
            }
        },
        else => {
            @compileError("Unsupported pointer size: " ++ @tagName(pT.size));
        },
    }
}

fn deserializeFloat(
    comptime fT: Type.Float,
    allocator: std.mem.Allocator,
    node: Node,
    options: ParseOptions,
) !@Type(Type{ .float = fT }) {
    _ = allocator;
    switch (fT.bits) {
        @bitSizeOf(f32), @bitSizeOf(f64) => {
            const mappings = [_]struct {
                tag: TAG,
                T: type,
            }{
                .{
                    .tag = .Float,
                    .T = f32,
                },
                .{
                    .tag = .Double,
                    .T = f64,
                },
            };

            inline for (mappings) |mapping| {
                if (@Type(Type{ .float = fT }) == mapping.T) {
                    if (std.meta.activeTag(node) == mapping.tag) {
                        return @field(node, @tagName(mapping.tag));
                    } else {
                        options.logErr(
                            "Wrong tag for type `{s}`, expected: {s}, got: {s}",
                            .{
                                @typeName(@Type(Type{ .float = fT })),
                                @tagName(mapping.tag),
                                @tagName(node),
                            },
                        );
                        return error.WrongSizedFloat;
                    }
                }
            }
            return error.NotInt;
        },
        else => {
            @compileError("Unsupported bit size for float");
        },
    }
}

fn deserializeInt(
    comptime iT: Type.Int,
    allocator: std.mem.Allocator,
    node: Node,
    options: ParseOptions,
) !@Type(Type{ .int = iT }) {
    _ = allocator;
    if (iT.signedness == .unsigned) {
        @compileError(
            "Unsigned integers are not supported in nbt, cannot use type: " //
            ++ @typeName(@Type(Type{ .int = iT })),
        );
    }
    switch (iT.bits) {
        @bitSizeOf(i8), @bitSizeOf(i16), @bitSizeOf(i32), @bitSizeOf(i64) => {
            const mappings = [_]struct {
                tag: TAG,
                T: type,
            }{
                .{
                    .tag = .Byte,
                    .T = i8,
                },
                .{
                    .tag = .Short,
                    .T = i16,
                },
                .{
                    .tag = .Int,
                    .T = i32,
                },
                .{
                    .tag = .Long,
                    .T = i64,
                },
            };

            inline for (mappings) |mapping| {
                if (@Type(Type{ .int = iT }) == mapping.T) {
                    if (std.meta.activeTag(node) == mapping.tag) {
                        return @field(node, @tagName(mapping.tag));
                    } else {
                        options.logErr(
                            "Wrong tag for type `{s}`, expected: {s}, got: {s}",
                            .{
                                @typeName(@Type(Type{ .int = iT })),
                                @tagName(mapping.tag),
                                @tagName(node),
                            },
                        );
                        return error.WrongSizedInt;
                    }
                }
            }
            return error.NotInt;
        },
        else => {
            @compileError("Unsupported bit size for int");
        },
    }
}

fn deserializeStruct(
    comptime T: type,
    allocator: std.mem.Allocator,
    node: Node,
    options: ParseOptions,
) !T {
    const sT: Type.Struct = @typeInfo(T).@"struct";
    if (std.meta.activeTag(node) != TAG.Compound) {
        return error.NotCompound;
    }
    const compound = node.Compound;

    var out: T = undefined;

    inline for (sT.fields) |field| {
        const found = fBlock: {
            for (compound) |value| {
                if (std.mem.eql(u8, field.name[0..], value.name[0..])) {
                    @field(out, field.name) = try deserializeNode(
                        field.type,
                        allocator,
                        value.tag,
                        options,
                    );
                    break :fBlock true;
                }
            }

            if (field.default_value) |dv| {
                @field(out, field.name) = @as(*const field.type, @ptrCast(@alignCast(dv))).*;
                break :fBlock true;
            }

            if (std.meta.activeTag(@typeInfo(field.type)) == .optional) {
                @field(out, field.name) = null;
                break :fBlock true;
            }

            break :fBlock false;
        };

        if (!found) {
            return error.FieldNotFound;
        }
    }

    return out;
}

fn deserializeArray(
    comptime T: type,
    allocator: std.mem.Allocator,
    node: Node,
    options: ParseOptions,
) !T {
    const aT: Type.Array = @typeInfo(T).array;

    var out: T = undefined;

    switch (node) {
        .ByteArray => |ba| {
            if (aT.child != u8) {
                options.logErr(
                    "Expected list of type: {s}, but found byte array",
                    .{@typeName(aT.child)},
                );
                return error.NotByteArray;
            }
            if (ba.len != aT.len) {
                options.logErr(
                    "Expected byte array of length: {}, but found length: {}",
                    .{
                        aT.len,
                        ba.len,
                    },
                );
                return error.IncorrectLen;
            }
            @memcpy(out[0..], ba[0..]);
        },
        .List => |l| {
            if (aT.child == u8) {
                unreachable;
            }
            for (0..out.len) |i| {
                out[i] = try deserializeNode(
                    aT.child,
                    allocator,
                    l.items[i],
                    options,
                );
            }
        },
        else => {
            options.logErr("Expected array node type found: {s}", .{
                @tagName(node),
            });
            return error.NotArrayNode;
        },
    }

    return out;
}
