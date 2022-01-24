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
};
