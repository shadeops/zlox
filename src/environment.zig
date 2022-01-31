const std = @import("std");
const Object = @import("object.zig").Object;
const Token = @import("token.zig").Token;

const EnvironmentError = error{
    UndefinedVariable,
};

pub const Environment = struct {
    const Self = @This();
    values: std.StringHashMap(?Object),
    enclosing: ?*Environment,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .values = std.StringHashMap(?Object).init(allocator),
            .enclosing = null,
        };
    }

    pub fn deinit(self: *Self) void {
        self.values.deinit();
        // leave the enclosing Environment the responsiblity of the caller
    }

    pub fn get(self: Self, name: Token) !?Object {
        if (self.values.contains(name.lexeme)) {
            return self.values.get(name.lexeme).?;
        }
        if (self.enclosing != null) return self.enclosing.?.get(name);
        return error.UnknownVariable;
    }

    pub fn assign(self: *Self, name: Token, value: ?Object) EnvironmentError!void {
        if (self.values.contains(name.lexeme)) {
            self.values.put(name.lexeme, value) catch {
                std.debug.print("Environment: failed to assign {s}\n", .{name});
            };
            return;
        }

        if (self.enclosing != null) {
            var parent: *Environment = self.enclosing.?;
            assign(parent, name, value) catch {
                std.debug.print("Environment: failed to assign to outer scope", .{});
            };
            return;
        }
        return error.UndefinedVariable;
    }

    pub fn define(self: *Self, name: []const u8, value: ?Object) void {
        self.values.put(name, value) catch {
            std.debug.print("Environment: failed to define {s}\n", .{name});
        };
    }
};
