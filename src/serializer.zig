const std = @import("std");

const ast = @import("ast.zig");
const NamedTag = ast.NamedTag;
const Node = ast.Node;
const TAG = ast.TAG;

pub const Args = struct {
    ignore_signedness: bool = false,
    pack_struct: bool = false,
};

pub fn toNamedTag(
    input: anytype,
    name: []const u8,
    allocator: std.mem.Allocator,
    comptime args: Args,
) !NamedTag {
    const new_name = try allocator.dupe(u8, name);

    const node = try toNode(input, allocator, args);

    return NamedTag{
        .name = new_name,
        .tag = node,
    };
}

fn toNode(
    input: anytype,
    allocator: std.mem.Allocator,
    comptime args: Args,
) !Node {
    const T: type = @TypeOf(input);
    const tInfo: std.builtin.Type = @typeInfo(T);

    switch (tInfo) {
        .int => {
            return intToNode(input, allocator, args);
        },
        .float => {
            return floatToNode(input, allocator, args);
        },
        .@"struct" => {
            return structToNode(input, allocator, args);
        },
        else => {
            @compileError("Unsupported type: " ++ @typeName(T));
        },
    }
}

fn intToNode(
    input: anytype,
    allocator: std.mem.Allocator,
    comptime args: Args,
) !Node {
    _ = allocator;
    const T: type = @TypeOf(input);
    const tInfo: std.builtin.Type = @typeInfo(T);

    const int = tInfo.int;

    if (!args.ignore_signedness and int.signedness == .unsigned) {
        @compileError(
            \\ NBT does not supported unsigned integers.
            \\ Set the `ignore_signedness` arg to true to enable casting.
        );
    }

    const signedTInfo = comptime ifo: {
        var signedTInfo = int;
        signedTInfo.signedness = .signed;
        break :ifo signedTInfo;
    };
    const signedT: type = @Type(.{ .int = signedTInfo });

    const singned_input: signedT = @bitCast(input);

    switch (int.bits) {
        8 => return Node{
            .Byte = singned_input,
        },
        16 => return Node{
            .Short = singned_input,
        },
        32 => return Node{
            .Int = singned_input,
        },
        64 => return Node{
            .Long = singned_input,
        },
        else => @compileError(std.fmt.comptimePrint(
            "Unsupported bit width: {}",
            .{int.bits},
        )),
    }

    unreachable;
}

test intToNode {
    const allocator = std.testing.allocator;

    inline for (.{ TAG.Byte, TAG.Short, TAG.Int, TAG.Long }) |tag| {
        const T: type = tag.typeOf();
        const input: T = 1;

        const node = try toNode(input, allocator, .{});

        const out = @field(node, @tagName(tag));

        try std.testing.expectEqual(input, out);
    }
}

fn floatToNode(
    input: anytype,
    allocator: std.mem.Allocator,
    comptime args: Args,
) !Node {
    _ = allocator;
    _ = args;
    const T: type = @TypeOf(input);
    const tInfo: std.builtin.Type = @typeInfo(T);

    const float = tInfo.float;

    switch (float.bits) {
        32 => return Node{
            .Float = input,
        },
        64 => return Node{
            .Double = input,
        },
        else => @compileError(std.fmt.comptimePrint(
            "Unsupported bit width: {}",
            .{float.bits},
        )),
    }

    unreachable;
}

test floatToNode {
    const allocator = std.testing.allocator;

    inline for (.{ TAG.Float, TAG.Double }) |tag| {
        const T: type = tag.typeOf();
        const input: T = 1;

        const node = try toNode(input, allocator, .{});

        const out = @field(node, @tagName(tag));

        try std.testing.expectEqual(input, out);
    }
}

fn structToNode(
    input: anytype,
    allocator: std.mem.Allocator,
    comptime args: Args,
) !Node {
    const T: type = @TypeOf(input);
    const tInfo: std.builtin.Type = @typeInfo(T);

    const str = tInfo.@"struct";
    if (args.pack_struct) {
        if (str.backing_integer) |IntT| {
            const val: IntT = @bitCast(input);
            return intToNode(val, allocator, args);
        }
    }

    var out: Node = .{
        .Compound = try allocator.alloc(NamedTag, str.fields.len),
    };

    errdefer allocator.free(out.Compound);

    inline for (str.fields, 0..) |field, i| {
        const name = field.name;
        const tag = try toNamedTag(@field(input, name), name, allocator, args);
        errdefer tag.deinit(allocator);
        out.Compound[i] = tag;
    }

    return out;
}

test structToNode {
    const T = struct {
        a: i8,
        b: i16,
        c: i32,
    };

    const input: T = .{
        .a = 1,
        .b = 2,
        .c = 3,
    };
    const allocator = std.testing.allocator;

    var out = try toNode(input, allocator, .{});
    defer out.deinit(allocator);

    const out_fields = out.Compound;

    outer: for (out_fields) |named_tag| {
        inline for (std.meta.fields(T)) |field| {
            if (std.mem.eql(u8, field.name, named_tag.name)) {
                const fieldTag = comptime TAG.ofType(field.type);

                const val: field.type = @field(named_tag.tag, @tagName(fieldTag));

                try std.testing.expectEqual(@field(input, field.name), val);
                continue :outer;
            }
        }
        unreachable;
    }
}
