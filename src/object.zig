const std = @import("std");

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

pub const ObjectData = union {
    number: f64,
    string: []const u8,
    boolean: bool,
    callable: *LoxCallable,
    nil: ?void,
};

pub const ObjectType = enum {
    NUMBER,
    STRING,
    BOOLEAN,
    CALLABLE,
    NIL,
};

pub const Object = struct {
    value: ObjectData,
    vtype: ObjectType,

    pub fn isType(self: Object, object_type: ObjectType) bool {
        return self.vtype == object_type;
    }

    pub fn initNumber(number: f64) Object {
        return .{
            .vtype = .NUMBER,
            .value = ObjectData{
                .number = number,
            },
        };
    }

    pub fn initString(string: []const u8) Object {
        return .{
            .vtype = .STRING,
            .value = ObjectData{
                .string = string,
            },
        };
    }

    pub fn initBoolean(boolean: bool) Object {
        return .{
            .vtype = .BOOLEAN,
            .value = ObjectData{
                .boolean = boolean,
            },
        };
    }

    pub fn initCallable(callable: *LoxCallable) Object {
        return .{
            .vtype = .CALLABLE,
            .value = ObjectData{
                .callable = callable,
            },
        };
    }

    pub fn initNil() Object {
        return .{
            .vtype = .NIL,
            .value = ObjectData{
                .nil = null,
            },
        };
    }

    pub fn toString(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        switch (self.vtype) {
            .STRING => return try std.fmt.allocPrint(allocator, "{s}", .{self.value.string}),
            .NUMBER => return try std.fmt.allocPrint(allocator, "{d}", .{self.value.number}),
            .BOOLEAN => return try std.fmt.allocPrint(allocator, "{}", .{self.value.boolean}),
            .CALLABLE => return self.value.callable.toString(),
            .NIL => return "nil",
        }
    }
};

test "Object.nil" {
    var o = Object.initNil();
    try std.testing.expect(o.value.nil == null);
}
