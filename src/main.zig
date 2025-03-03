//! By convention, main.zig is where your main function lives in the case that
//! you are building an executable. If you are making a library, the convention
//! is to delete this file and start with root.zig instead.
const std = @import("std");
const root = @import("root.zig");

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    var argsIter = std.process.args();
    _ = argsIter.next();
    if (argsIter.next()) |input| {
        std.debug.print("Input file: {s}\n", .{input});
        var inFile = try std.fs.cwd().openFile(input, std.fs.File.OpenFlags{
            .mode = .read_only,
        });
        try inFile.seekTo(0);
        defer inFile.close();
        var decompressedData = std.ArrayList(u8).init(allocator);
        defer decompressedData.deinit();
        try std.compress.gzip.decompress(inFile.reader(), decompressedData.writer());
        const out = try root.parseFromBytes(decompressedData.items, allocator);
        try out.fancyPrintStdOut();
    }
}

test "simple test" {
    var list = std.ArrayList(i32).init(std.testing.allocator);
    defer list.deinit(); // Try commenting this out and see if zig detects the memory leak!
    try list.append(42);
    try std.testing.expectEqual(@as(i32, 42), list.pop());
}

test "fuzz example" {
    const global = struct {
        fn testOne(input: []const u8) anyerror!void {
            // Try passing `--fuzz` to `zig build test` and see if it manages to fail this test case!
            try std.testing.expect(!std.mem.eql(u8, "canyoufindme", input));
        }
    };
    try std.testing.fuzz(global.testOne, .{});
}
