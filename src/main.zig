const std = @import("std");

const Scanner = @import("scanner.zig").Scanner;
const Token = @import("token.zig").Token;
const Parser = @import("parser.zig").Parser;
const ParseError = @import("parser.zig").ParseError;
const Resolver = @import("resolver.zig").Resolver;
const Interpreter = @import("interpreter.zig").Interpreter;

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
        runFile(arena.allocator(), args[1]);
    } else {
        runPrompt(arena.allocator());
    }
}

fn runFile(allocator: std.mem.Allocator, path: []const u8) void {
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

    run(allocator, source);

    if (had_error)
        std.process.exit(65);
    if (had_runtime_error)
        std.process.exit(70);

    return;
}

fn runPrompt(allocator: std.mem.Allocator) void {
    const stdout = std.io.getStdOut();
    const stdin = std.io.getStdIn();
    const buf_size = 1024;
    // NOTES:
    //  * We can't use a buffer because we many of our objects reference slices
    //      of the input stream. Until the various code paths are updated to take ownership
    //      we'll use the allocator.
    //var buffer: [1024]u8 = undefined;
    while (true) {
        stdout.writeAll("> ") catch {
            std.log.err("Unable to output to prompt.", .{});
            return;
        };
        const input = stdin.reader().readUntilDelimiterAlloc(allocator, '\n', buf_size) catch |err| {
            switch (err) {
                error.StreamTooLong => {
                    std.log.err("Input too big (more than {} characters)", .{buf_size});
                    continue;
                },
                error.EndOfStream => return,
                else => {
                    std.log.err("Error reading input: '{s}'.", .{@errorName(err)});
                    return;
                },
            }
        };
        if (input.len == 0) break;
        run(allocator, input);
        had_error = false;
        had_runtime_error = false;
    }
    return;
}

/// NOTES:
///  * We can't deinit the scanner or tokens here as this function needs to be
///     reentered when using the REPL (runPrompt)
fn run(allocator: std.mem.Allocator, source: []const u8) void {
    std.log.debug("Starting scanner", .{});
    var token_scanner = Scanner.init(allocator, source);
    var tokens = token_scanner.scanTokens();

    std.log.debug("Starting parser", .{});
    var parser = Parser.init(allocator, &tokens);
    var statements = parser.parse() catch |err| {
        switch (err) {
            ParseError.Paren, ParseError.Expression => {},
            ParseError.OutOfMemory => {
                std.log.err("Ran out of memory parsing", .{});
            },
            else => {
                std.log.err("Unknown Parsing Error\n", .{});
                // There shouldn't be another error type raised
                unreachable;
            },
        }
        return;
    };

    if (had_error) return;

    std.log.debug("Starting resolver", .{});
    var resolver = Resolver.init(allocator, &interpreter);
    resolver.resolve(statements) catch {
        std.log.err("resolving error", .{});
        return;
    };

    if (had_error) return;

    std.log.debug("Starting interpreter", .{});
    interpreter.interpret(statements);

    return;
}

// In jlox this is error(int, String);
pub fn lineError(line: u32, message: []const u8) void {
    report(line, "", message);
}

fn report(line: u32, where: []const u8, message: []const u8) void {
    std.log.err("[line {}] Error{s}: {s}", .{ line, where, message });
    had_error = true;
}

// In jlox this is error(Token, String);
pub fn tokenError(token: *const Token, message: []const u8) void {
    if (token.token_type == .EOF) {
        report(token.line, " at end", message);
    } else {
        // NOTE: assume lexeme is less than 1024 bytes.
        var buf: [1024]u8 = undefined;
        report(
            token.line,
            std.fmt.bufPrint(buf[0..], " at '{s}'", .{token.lexeme}) catch {
                std.log.err("Failed to print to buffer.", .{});
                return;
            },
            message,
        );
    }
}

// We diverge slightly from jlox, their runtime errors print a message
// then on a separate new line supply the line number. We'll split this
// in two. At the call site we'll issue a std.log.err, then call this
// function to report the line number and set the had_runtime_error flag.
// The main reason for doing this is to avoid having to do string process.

// As a TODO it might be useful to wrap all this up into one function.
pub fn runtimeError(token: *const Token) void {
    std.log.err("[line {}]", .{token.line});
    had_runtime_error = true;
}
