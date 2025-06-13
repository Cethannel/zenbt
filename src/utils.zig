pub fn assertEq(expected: anytype, actual: anytype) void {
    const expectedT = @TypeOf(expected);
    const actualT = @TypeOf(actual);
    comptime if (expectedT != actualT) {
        @compileError(
            "Types do not match\nExpected: " ++ @typeName(expectedT) ++ //
                "\nActual" ++ @typeName(actualT),
        );
    };

    if (expected == actual) {
        return;
    }

    std.testing.expectEqual(expected, actual) catch unreachable;
}

const std = @import("std");
