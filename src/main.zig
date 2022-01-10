const std = @import("std");
const scanner = @import("scanner.zig");

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len > 2) {
        std.log.err("Usage: zlox [script]", .{});
        std.process.exit(64);
    } else if (args.len == 2) {
        try runFile(allocator, args[1]);
    } else {
        try runPrompt(allocator);
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    std.log.info("Running {s}", .{path});
    var file = std.fs.cwd().openFile(path, .{.read=true, .write=false}) catch |err| {
        std.log.err("Could not open {s}, error was {s}", .{path, @errorName(err)});
        return;
    };
    defer file.close();
    var source = file.readToEndAlloc(allocator, 1024*1024*1024) catch |err| {
        switch (err) {
            error.FileTooBig => {
                std.log.err("Could not open {s}, bigger than 1GB", .{path});
                return;
            },
            else => unreachable,
        }
    };
    defer allocator.free(source);
    
    try run(allocator, source);
    return;
}

fn runPrompt(allocator: std.mem.Allocator) !void {
    _ = allocator;
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();
    var buffer: [1024]u8 = undefined;
    while(true) {
        try stdout.writeAll("> ");
        const input = stdin.reader().readUntilDelimiter(&buffer, '\n') catch |err| {
            switch (err) {
                error.StreamTooLong => {
                    std.log.err("Input too big (more than 1024 characters)", .{});
                    continue;
                },
                error.EndOfStream => return,
                else => return err,
                }
        };
        if (input.len == 0 ) break;
        try stdout.writer().print("Input was: {s}\n", .{input});
        try run(allocator, input);
    }
    return;
}

fn run(allocator: std.mem.Allocator, source: []const u8) !void {
    var token_scanner = try scanner.Scanner.init(allocator, source);
    var tokens = try token_scanner.scanTokens();
    std.log.info("Scanned tokens:", .{});
    for (tokens.items) |item, i| {
        std.log.info("  {}: {s}", .{i, item.lexeme});
    }
    return;
}

test "basic test" {
    const s = "hello there";
    var slice: []const u8 = s[0..5];
    std.debug.print("{s}", .{slice[0..5]});
    try std.testing.expectEqual(slice.len, 5);
}
