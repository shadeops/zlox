const std = @import("std");
const Object = @import("object.zig").Object;
const LoxClass = @import("class.zig").LoxClass;
const LoxCallable = @import("callable.zig").LoxCallable;
const Token = @import("token.zig").Token;

pub const LoxInstance = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    class: *const LoxClass,
    fields: std.StringHashMap(Object),

    pub fn init(
        allocator: std.mem.Allocator,
        class: *const LoxClass,
        fields: std.StringHashMap(Object),
    ) Self {
        return .{
            .allocator = allocator,
            .class = class,
            .fields = fields,
        };
    }

    pub fn deinit(self: *Self) void {
        self.fields.deinit();
    }

    pub fn create(allocator: std.mem.Allocator, class: *const LoxClass) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(allocator, class, std.StringHashMap(Object).init(allocator));
        return ptr;
    }

    pub fn set(self: *Self, name: Token, value: Object) !void {
        try self.fields.put(name.lexeme, value);
    }

    pub fn get(self: *Self, name: Token) !Object {
        if (self.fields.contains(name.lexeme)) {
            return self.fields.get(name.lexeme).?;
        }

        var method = self.class.findMethod(name.lexeme) orelse {
            std.log.err("Undefined property, '{s}'.", .{name.lexeme});
            return error.RuntimeError;
        };

        // findMethod gives us a *LoxFunction, but we need to return a
        // Object annoyingly.
        var callable_ptr = self.allocator.create(LoxCallable) catch unreachable;
        callable_ptr.* = method.bind(self).toCallable();
        return Object.initCallable(callable_ptr);
    }

    pub fn toString(self: Self) []const u8 {
        // TODO add instance;
        return self.class.name;
    }
};
