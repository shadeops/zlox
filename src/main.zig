const std = @import("std");
const scanner = @import("scanner.zig");
const ast_printer = @import("ast_printer.zig");

const Interpreter = @import("interpreter.zig").Interpreter;
const Parser = @import("parser.zig").Parser;
const Token = @import("token.zig").Token;

var had_error: bool = false;
var had_runtime_error: bool = false;

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
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
    var file = std.fs.cwd().openFile(path, .{ .read = true, .write = false }) catch |err| {
        std.log.err("Could not open {s}, error was {s}", .{ path, @errorName(err) });
        return;
    };
    defer file.close();
    var source = file.readToEndAlloc(allocator, 1024 * 1024 * 1024) catch |err| {
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

    if (had_error)
        std.process.exit(65);
    if (had_runtime_error)
        std.process.exit(70);

    return;
}

fn runPrompt(allocator: std.mem.Allocator) !void {
    _ = allocator;
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();
    var buffer: [1024]u8 = undefined;
    while (true) {
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
        if (input.len == 0) break;
        try run(allocator, input);
        had_error = false;
        had_runtime_error = false;
    }
    return;
}

fn run(allocator: std.mem.Allocator, source: []const u8) !void {
    var token_scanner = try scanner.Scanner.init(allocator, source);
    defer token_scanner.deinit();
    var tokens = try token_scanner.scanTokens();
    //for (tokens.items) |t| {
    //    std.debug.print("{s}\n", .{t.lexeme});
    //}

    var parser_arena = std.heap.ArenaAllocator.init(allocator);
    defer parser_arena.deinit();
    var parser = Parser.init(parser_arena.allocator(), tokens);
    var expression = parser.parse() orelse {
        std.log.err("parsing error\n", .{});
        return;
    };

    if (had_error) return;

    //var printer = ast_printer.AstPrinter.init(allocator);
    //defer printer.deinit();
    //try printer.print(expression);

    var interpreter = Interpreter.init(parser_arena.allocator());
    defer interpreter.deinit();
    interpreter.interpret(expression);

    return;
}

pub fn lineError(line: u32, message: []const u8) void {
    report(line, "", message);
}

pub fn tokenError(token: Token, message: []const u8) !void {
    if (token.token_type == .EOF) {
        report(token.line, " at end", message);
    } else {
        // NOTE: assume lexeme is less than 1024 bytes.
        var buf: [1024]u8 = undefined;
        report(
            token.line,
            try std.fmt.bufPrint(buf[0..], " at '{s}'", .{token.lexeme}),
            message,
        );
    }
}

pub fn runtimeError(message: []const u8) void {
    std.log.err("{s}\n[line ?]", .{message});
    had_runtime_error = true;
}

pub fn report(line: u32, where: []const u8, message: []const u8) void {
    std.log.err("[line {}] Error{s}: {s}", .{ line, where, message });
    had_error = true;
}

test "basic test" {
    const s = "hello there";
    var slice: []const u8 = s[0..5];
    std.debug.print("{s}", .{slice[0..5]});
    try std.testing.expectEqual(slice.len, 5);
}
