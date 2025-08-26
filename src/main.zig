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
        var decompressedData = try std.ArrayList(u8).initCapacity(allocator, 1024 * 1024);
        defer decompressedData.deinit(allocator);
        const file_buffer = try allocator.alloc(u8, 1024);
        defer allocator.free(file_buffer);
        const decompress_buffer = try allocator.alloc(u8, 1024);
        defer allocator.free(decompress_buffer);
        var fileReader = inFile.reader(file_buffer);
        var decompress = std.compress.flate.Decompress.init(&fileReader.interface, .gzip, decompress_buffer);
        const out = try root.parseFromReader(&decompress.reader, allocator, .{
            .printErrors = true,
        });
        try out.fancyPrintStdOut();
    }
}
