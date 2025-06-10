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
        std.compress.gzip.decompress(inFile.reader(), decompressedData.writer()) catch {
            decompressedData.clearAndFree();
            try inFile.seekTo(0);
            try inFile.reader().readAllArrayList(&decompressedData, 1024 * 1024 * 1024);
        };
        const out = try root.parseFromBytes(decompressedData.items, allocator, .{
            .printErrors = true,
        });
        try out.fancyPrintStdOut();
    }
}
