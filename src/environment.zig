const std = @import("std");
const Object = @import("object.zig").Object;
const Token = @import("token.zig").Token;

const runtimeError = @import("main.zig").runtimeError;

const EnvironmentError = error{
    UndefinedVariable,
    UnknownVariable,
};

pub const Environment = struct {
    const Self = @This();
    values: std.StringHashMap(Object),
    enclosing: ?*Environment,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .values = std.StringHashMap(Object).init(allocator),
            .enclosing = null,
        };
    }

    /// NOTES:
    ///   * Cleanup of self.enclosing Environment the responsiblity of the caller
    pub fn deinit(self: *Self) void {
        self.values.deinit();
    }

    pub fn get(self: Self, name: Token) EnvironmentError!Object {

        //std.debug.print("getting: {s} in {*}\n", .{name.lexeme, &self});
        //var it = self.values.keyIterator();
        //while (it.next()) |k| {
        //    std.debug.print(" key: {s}\n", .{k.*});
        //}

        if (self.values.contains(name.lexeme)) {
            return self.values.get(name.lexeme).?;
        }
        if (self.enclosing != null) return self.enclosing.?.get(name);
        std.log.err("Undefined variable '{s}'.", .{name.lexeme});
        runtimeError(name);
        return error.UnknownVariable;
    }

    pub fn assign(self: *Self, name: Token, value: Object) EnvironmentError!void {
        if (self.values.contains(name.lexeme)) {
            self.values.put(name.lexeme, value) catch {
                std.log.err("Environment: failed to assign {s}\n", .{name});
            };
            return;
        }

        if (self.enclosing != null) {
            var parent: *Environment = self.enclosing.?;
            assign(parent, name, value) catch {
                std.log.err("Environment: failed to assign to outer scope", .{});
            };
            return;
        }
        std.log.err("Undefined variable '{s}'.", .{name.lexeme});
        runtimeError(name);
        return error.UndefinedVariable;
    }

    pub fn define(self: *Self, name: []const u8, value: Object) void {
        self.values.put(name, value) catch {
            std.log.err("Environment: failed to define {s}\n", .{name});
        };
    }

    pub fn getAt(self: *Self, distance: usize, name: []const u8) !Object {
        return self.ancestor(distance).values.get(name).?;
    }

    pub fn assignAt(self: *Self, distance: usize, name: Token, value: Object) void {
        return self.ancestor(distance).values.put(name.lexeme, value) catch {
            std.log.err("Environment: failed to put {s}\n", .{name.lexeme});
        };
    }

    fn ancestor(self: *Self, distance: usize) *Environment {
        var environment = self;
        var i: usize = 0;
        while (i < distance) : (i += 1) {
            environment = environment.enclosing.?;
        }
        return environment;
    }
};
