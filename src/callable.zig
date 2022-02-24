const std = @import("std");
const Object = @import("object.zig").Object;
const Interpreter = @import("interpreter.zig").Interpreter;

/// NOTE:
///     This is needed due to an "instanceof" check in Interpreter.visitClassStmt
//      TODO: This could probably be replaced with a tagged union.
pub const CallableType = enum {
    FUNCTION,
    CLASS,
};

pub const LoxCallable = struct {
    impl: *const anyopaque,
    arity: u8,
    callable_type: CallableType,

    callFn: fn (*const anyopaque, *Interpreter, *const std.ArrayList(Object)) anyerror!Object,
    toStringFn: fn (*const anyopaque, allocator: std.mem.Allocator) anyerror![]const u8,

    pub fn call(
        callable: *const LoxCallable,
        interpreter: *Interpreter,
        arguments: *const std.ArrayList(Object),
    ) anyerror!Object {
        return callable.callFn(callable.impl, interpreter, arguments);
    }

    pub fn toString(callable: *const LoxCallable, allocator: std.mem.Allocator) anyerror![]const u8 {
        return try callable.toStringFn(callable.impl, allocator);
    }
};
