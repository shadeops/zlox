const std = @import("std");
const TokenType = @import("token_types.zig").TokenType;
const Object = @import("object.zig").Object;

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    literal: ?Object,
    line: u32,

    pub fn init(self: *Token, token_type: TokenType, lexeme: []const u8, literal: Object, line: u32) void {
        self.token_type = token_type;
        self.lexeme = lexeme;
        self.literal = literal;
        self.line = line;
    }

    pub fn toString(self: Token, allocator: std.mem.Allocator) ![]const u8 {
        if (self.token_type == .STRING) {
            return try std.fmt.allocPrint(allocator, "{s}", .{self.literal.?.string});
        } else {
            return try std.fmt.allocPrint(allocator, "{d}", .{self.literal.?.number});
        }
    }
};
