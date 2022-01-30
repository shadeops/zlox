const std = @import("std");

pub const ObjectData = union {
    number: f64,
    string: []const u8,
    boolean: bool,
};

pub const ObjectType = enum {
    NUMBER,
    STRING,
    BOOLEAN,
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

    pub fn toString(self: Object, allocator: std.mem.Allocator) ![]const u8 {
        switch (self.vtype) {
            .STRING => return try std.fmt.allocPrint(allocator, "{s}", .{self.value.string}),
            .NUMBER => return try std.fmt.allocPrint(allocator, "{d}", .{self.value.number}),
            .BOOLEAN => return try std.fmt.allocPrint(allocator, "{}", .{self.value.boolean}),
        }
    }
};
