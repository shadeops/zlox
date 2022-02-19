const std = @import("std");

const LoxInstance = @import("instance.zig").LoxInstance;
const LoxCallable = @import("callable.zig").LoxCallable;
const Interpreter = @import("interpreter.zig").Interpreter;

// Due to https://github.com/ziglang/zig/issues/4562 I don't seem to be able to use
// optionals here.
// This fails
// const Func = fn (*anyopaque, std.ArrayList(?*const Object)) *Object;
// but this works
//const Func = fn (*anyopaque, std.ArrayList(*const Object)) *Object;

// NOTE:
//  LoxCallable needs to be a pointer because it makes use of an Object as part of
//  its argument list and return type, which appears to cause a self reference error
//  at compile time. The error is a bit vague at this point so it will be interesting
//  to revisit later in Zig's development.
//  TODO  I'm not entirely convinced this is a real problem and might just be a
//  stage 1 bug. Investigate more and determine if it is a misunderstanding on my
//  part (very likely) or a current limitiation/bug.

pub const ObjectType = enum {
    number,
    string,
    boolean,
    callable,
    instance,
    nil,
};

pub const Object = union(ObjectType) {
    number: f64,
    string: []const u8,
    boolean: bool,
    callable: *const LoxCallable,
    instance: *LoxInstance,
    nil: void,

    pub fn isSameType(self: Object, object: Object) bool {
        return @as(ObjectType, self) == @as(ObjectType, object);
    }

    pub fn initNumber(number: f64) Object {
        return .{
            .number = number,
        };
    }

    pub fn initString(string: []const u8) Object {
        return .{
            .string = string,
        };
    }

    pub fn initBoolean(boolean: bool) Object {
        return .{
            .boolean = boolean,
        };
    }

    pub fn initCallable(callable: *const LoxCallable) Object {
        return .{
            .callable = callable,
        };
    }

    pub fn initInstance(instance: *LoxInstance) Object {
        return .{
            .instance = instance,
        };
    }

    pub fn initNil() Object {
        return Object.nil;
    }

    pub fn toString(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        switch (self) {
            .string => |value| return try std.fmt.allocPrint(allocator, "{s}", .{value}),
            .number => |value| return try std.fmt.allocPrint(allocator, "{d}", .{value}),
            .boolean => |value| return try std.fmt.allocPrint(allocator, "{}", .{value}),
            .callable => |value| return value.toString(),
            .instance => |value| return value.toString(),
            .nil => return "nil",
        }
    }
};

test "Object.nil" {
    var o = Object.initNil();
    try std.testing.expect(o == .nil);
}
