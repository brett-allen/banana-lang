const std = @import("std");
const lex = @import("lex.zig");
const parser = @import("parser.zig");

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
    // var env = object.Environment.init(heap);
    // defer env.deinit();

    // var eval = evaluator.Evaluator.init(heap);
    // var result = try eval.evalProgram(&program, env);
    // defer result.deinit(heap);

    // Print the result (skip null - it's not meaningful output)
    // switch (result) {
    //     .null => {}, // Don't print null results
    //     else => {
    //         var stdout_buffer: [4096]u8 = undefined;
    //         var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    //         const writer = &stdout_writer.interface;
    //         try result.print(writer);
    //         try writer.print("\n", .{});
    //         writer.flush() catch {};
    //     },
    // }
}
