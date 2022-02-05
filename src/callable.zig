const std = @import("std");
const Object = @import("object.zig").Object;
const Interpreter = @import("interpreter.zig").Interpreter;

pub const LoxCallable = struct {
    impl: *const anyopaque,
    arity: u8,

    callFn: fn (*const anyopaque, *Interpreter, std.ArrayList(Object)) anyerror!Object,
    toStringFn: fn (*const anyopaque) []const u8,

    pub fn call(callable: *const LoxCallable, interpreter: *Interpreter, arguments: std.ArrayList(Object)) anyerror!Object {
        return callable.callFn(callable.impl, interpreter, arguments);
    }

    pub fn toString(callable: *const LoxCallable) []const u8 {
        return callable.toStringFn(callable.impl);
    }
};
