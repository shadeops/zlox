const std = @import("std");
const scanner = @import("scanner.zig");
const ast_printer = @import("ast_printer.zig");

const Interpreter = @import("interpreter.zig").Interpreter;
const Parser = @import("parser.zig").Parser;
const Resolver = @import("resolver.zig").Resolver;
const Token = @import("token.zig").Token;

//pub const log_level: std.log.Level = .debug;
pub const log_level: std.log.Level = .info;

var interpreter: Interpreter = undefined;

var had_error: bool = false;
var had_runtime_error: bool = false;

/// NOTES:
///  * There is some cheating going on with memory. By using the arena allocator
///     we are allocating without worrying about cleaning up. This isn't great
///     as we hang onto everything until the end of execution but simplifies
///     the code for now.
// TODO: Investigate being more rigorous with memory clean-up instead of relying
//  on the arena.
pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    interpreter = Interpreter.init(arena.allocator());
    defer interpreter.deinit();

    if (args.len > 2) {
        std.log.err("Usage: zlox [script]", .{});
        std.process.exit(64);
    } else if (args.len == 2) {
        try runFile(arena.allocator(), args[1]);
    } else {
        try runPrompt(arena.allocator());
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) !void {
    std.log.debug("Running {s}", .{path});
    var file = std.fs.cwd().openFile(path, .{}) catch |err| {
        std.log.err("Could not open {s}, error was {s}", .{ path, @errorName(err) });
        return;
    };
    defer file.close();
    var source = file.readToEndAlloc(allocator, 1024 * 1024 * 1024) catch |err| {
        switch (err) {
            error.FileTooBig => {
                std.log.err("Could not open {s}, larger than 1GB", .{path});
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
    const buf_size = 1024;
    // NOTES:
    //  * We can't use a buffer because we many of our objects reference slices
    //      of the input stream. Until the various code paths are updated to take ownership
    //      we'll use the allocator.
    //var buffer: [1024]u8 = undefined;
    while (true) {
        try stdout.writeAll("> ");
        const input = stdin.reader().readUntilDelimiterAlloc(allocator, '\n', buf_size) catch |err| {
            switch (err) {
                error.StreamTooLong => {
                    std.log.err("Input too big (more than {} characters)", .{buf_size});
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

/// NOTES:
///  * We can't deinit the scanner or tokens here as this function needs to be
///     reentered when using the REPL (runPrompt)
fn run(allocator: std.mem.Allocator, source: []const u8) !void {
    var token_scanner = try scanner.Scanner.init(allocator, source);

    std.log.debug("Starting scanner", .{});
    var tokens = try token_scanner.scanTokens();

    std.log.debug("Starting parser", .{});
    var parser = Parser.init(allocator, tokens);
    var statements = parser.parse() catch {
        std.log.err("parsing error\n", .{});
        return;
    };

    if (had_error) return;

    std.log.debug("Starting resolver", .{});
    var resolver = Resolver.init(allocator, &interpreter);
    resolver.resolveStmts(statements) catch {
        std.log.err("resolving error", .{});
        return;
    };

    if (had_error) return;

    std.log.debug("Starting interpreter", .{});
    interpreter.interpret(statements);

    return;
}

// TODO: Audit the error workflow. Currently this is possibly
// out of sync with what jlox is doing partly because we can't
// easily capture extra data as with Java when raising excepions.
// Currently the approach isn't consistent.
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
