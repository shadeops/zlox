const std = @import("std");
const Object = @import("object.zig").Object;
const LoxCallable = @import("callable.zig").LoxCallable;
const LoxInstance = @import("instance.zig").LoxInstance;
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
    closure: *Environment,
    is_initializer: bool,

    pub fn init(
        declaration: Stmt.Function,
        closure: *Environment,
        is_initializer: bool,
    ) Self {
        return .{
            .declaration = declaration,
            .closure = closure,
            .is_initializer = is_initializer,
        };
    }

    pub fn create(
        allocator: std.mem.Allocator,
        declaration: Stmt.Function,
        closure: *Environment,
        is_initializer: bool,
    ) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(declaration, closure, is_initializer);
        return ptr;
    }

    pub fn toCallable(self: *Self) LoxCallable {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .arity = @intCast(u8, self.declaration.params.items.len),
            .callable_type = .FUNCTION,
            .callFn = call,
            .toStringFn = toString,
        };
    }

    pub fn bind(self: Self, instance: *LoxInstance) *Self {
        // Same as with call() we'll hijack LoxInstance's allocator
        var environment = instance.allocator.create(Environment) catch unreachable;
        environment.* = Environment.init(instance.allocator);
        environment.enclosing = self.closure;
        environment.define("this", Object.initInstance(instance));
        return Self.create(
            instance.allocator,
            self.declaration,
            environment,
            self.is_initializer,
        );
    }

    pub fn call(
        ptr: *const anyopaque,
        interpreter: *Interpreter,
        arguments: std.ArrayList(Object),
    ) anyerror!Object {
        var self = castToConstSelf(Self, ptr);

        // using the interpreter's is probably bad form
        var environment = interpreter.allocator.create(Environment) catch unreachable;
        defer interpreter.allocator.destroy(environment);
        environment.* = Environment.init(interpreter.allocator);
        environment.enclosing = self.closure;

        for (arguments.items) |arg, i| {
            environment.define(self.declaration.params.items[i].lexeme, arg);
        }

        // This mimics what jlox is doing; using a "exception" for control flow.
        // Here we validate that it isn't null (which it shoudln't be) then if it
        // was a ReturnValue "error" we are exiting a call(), so return the stashed
        // interpreter.ret otherwise bubble up whatever error it was.
        interpreter.executeBlock(self.declaration.body, environment) catch |err| {
            switch (err) {
                error.ReturnValue => {
                    if (self.is_initializer)
                        return self.closure.getAt(0, "this");
                    return interpreter.ret orelse return err;
                },
                else => return err,
            }
        };
        if (self.is_initializer) return self.closure.getAt(0, "this");
        return Object.initNil();
    }

    fn toString(ptr: *const anyopaque, allocator: std.mem.Allocator) ![]const u8 {
        var self = castToConstSelf(Self, ptr);
        return try std.fmt.allocPrint(
            allocator,
            "<fn {s}>",
            .{self.declaration.name.lexeme},
        );
    }
};
