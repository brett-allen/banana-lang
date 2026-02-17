const std = @import("std");
const lex = @import("lex.zig");
const parser = @import("parser.zig");
const evaluator = @import("evaluator.zig");

pub fn main() !void {
    // get an allocator; standard zig memory management
    var alloc = std.heap.ArenaAllocator.init(std.heap.page_allocator);
    defer _ = alloc.deinit();

    // create a heap allocator; this is the allocator that will be used for all memory allocations
    const heap = alloc.allocator();

    // get the arguments from the command line
    const args = try std.process.argsAlloc(heap);
    defer std.process.argsFree(heap, args);

    if (args.len < 2) {
        std.debug.print("Usage: {s} <file>\n", .{args[0]});
        return;
    }

    // read the file contents into a string
    const file_contents = try std.fs.cwd().readFileAlloc(heap, args[1], std.math.maxInt(usize));
    defer heap.free(file_contents);

    // Parse the program
    const lexer = lex.Lexer.init(file_contents);

    var p = parser.Parser.init(heap, lexer);
    var program = try p.parseProgram();

    const stdout_buffer = try heap.alloc(u8, 4096);
    defer heap.free(stdout_buffer);
    var stdout_writer = std.fs.File.stdout().writer(stdout_buffer);
    const writer = &stdout_writer.interface;
    try program.string(writer);
    try writer.flush();

    std.debug.print("\n", .{});

    // Evaluate the program
    var eval = evaluator.Evaluator.init(heap);
    defer eval.deinit();

    const result = try eval.evaluate(.{ .program = program });

    // Print the result (skip null - it's not meaningful output)
    if (result) |obj| {
        switch (obj) {
            .integer => |int_obj| {
                std.debug.print("{}\n", .{int_obj.value});
            },
            .@"null" => {}, // Don't print null results
            else => {
                // Print other object types
                const stdout_buffer2 = try heap.alloc(u8, 4096);
                defer heap.free(stdout_buffer2);
                var stdout_writer2 = std.fs.File.stdout().writer(stdout_buffer2);
                const writer2 = &stdout_writer2.interface;
                try obj.inspect(writer2);
                try writer2.flush();
            },
        }
    }
}
