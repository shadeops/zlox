const std = @import("std");
const Object = @import("object.zig").Object;
const LoxCallable = @import("callable.zig").LoxCallable;
const LoxFunction = @import("function.zig").LoxFunction;
const LoxInstance = @import("instance.zig").LoxInstance;
const Interpreter = @import("interpreter.zig").Interpreter;
const Environment = @import("environment.zig").Environment;
//const Token = @import("token.zig").Token;
//const Expr = @import("expr.zig");
//const Stmt = @import("stmt.zig");
//const Interpreter = @import("interpreter.zig").Interpreter;
//
//const tokenError = @import("main.zig").tokenError;

fn castToConstSelf(comptime T: type, ptr: *const anyopaque) *const T {
    const alignment = @alignOf(T);
    const self = @ptrCast(*const T, @alignCast(alignment, ptr));
    return self;
}

pub const LoxClass = struct {
    const Self = @This();
    name: []const u8,
    superclass: ?*const LoxClass,
    methods: std.StringHashMap(*LoxFunction),

    pub fn init(
        name: []const u8,
        superclass: ?*const LoxClass,
        methods: std.StringHashMap(*LoxFunction),
    ) LoxClass {
        return .{
            .name = name,
            .superclass = superclass,
            .methods = methods,
        };
    }

    pub fn create(
        allocator: std.mem.Allocator,
        name: []const u8,
        superclass: ?*const LoxClass,
        methods: std.StringHashMap(*LoxFunction),
    ) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(name, superclass, methods);
        return ptr;
    }

    pub fn toCallable(self: *const Self) LoxCallable {
        // We diverge slightly from jlox, where arity is method,
        // here we have it as a field so move the evaluation here.
        var initializer = self.findMethod("init");
        var arity: u8 = 0;
        if (initializer != null) {
            // TODO this is a mess, we should probably have a
            // Function.arity() instead of stuffing it in the Callable
            arity = @intCast(u8, initializer.?.declaration.params.items.len);
        }
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .arity = arity,
            .callable_type = .CLASS,
            .callFn = call,
            .toStringFn = toString,
        };
    }

    pub fn findMethod(self: Self, name: []const u8) ?*LoxFunction {
        if (self.methods.contains(name)) {
            return self.methods.get(name);
        }

        if (self.superclass != null) {
            return self.superclass.?.findMethod(name);
        }

        return null;
    }

    fn call(
        ptr: *const anyopaque,
        interpreter: *Interpreter,
        arguments: std.ArrayList(Object),
    ) anyerror!Object {
        var self = castToConstSelf(Self, ptr);

        var instance = LoxInstance.create(interpreter.allocator, self);

        var initializer = self.findMethod("init");
        if (initializer != null) {
            _ = try initializer.?.bind(instance).call(interpreter, arguments);
        }

        return Object.initInstance(instance);
    }

    fn toString(ptr: *const anyopaque) []const u8 {
        var self = castToConstSelf(Self, ptr);
        return self.name;
    }
};
