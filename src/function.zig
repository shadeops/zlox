const std = @import("std");
const Object = @import("object.zig").Object;
const LoxCallable = @import("callable.zig").LoxCallable;
const Interpreter = @import("interpreter.zig").Interpreter;
const Environment = @import("environment.zig").Environment;
const Stmt = @import("stmt.zig");

fn castToConstSelf(comptime T: type, ptr: *const anyopaque) *const T {
    const alignment = @alignOf(T);
    const self = @ptrCast(*const T, @alignCast(alignment, ptr));
    return self;
}

pub const LoxFunction = struct {
    const Self = @This();
    declaration: Stmt.Function,

    pub fn init(declaration: Stmt.Function) Self {
        return .{
            .declaration = declaration,
        };
    }

    pub fn create(allocator: std.mem.Allocator, declaration: Stmt.Function) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(declaration);
        return ptr;
    }

    pub fn toCallable(self: *Self) LoxCallable {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .arity = @intCast(u8, self.declaration.params.items.len),
            .callFn = call,
            .toStringFn = toString,
        };
    }

    fn call(
        ptr: *const anyopaque,
        interpreter: *Interpreter,
        arguments: std.ArrayList(Object),
    ) anyerror!Object {
        var self = castToConstSelf(Self, ptr);

        var environment = interpreter.allocator.create(Environment) catch unreachable;
        // using the interpreter's is probably bad form
        environment.* = Environment.init(interpreter.allocator);

        for (arguments.items) |arg, i| {
            environment.define(self.declaration.params.items[i].lexeme, arg);
        }

        try interpreter.executeBlock(self.declaration.body, environment);
        return Object.initNil();
    }

    fn toString(ptr: *const anyopaque) []const u8 {
        // todo allocation
        _ = ptr;
        return "<fn declaration.name.lexeme>";
    }
};
