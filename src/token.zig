const std = @import("std");
const TokenType = @import("token_types.zig").TokenType;
const Object = @import("object.zig").Object;

/// NOTES:
///  * The literal can null or an Object. While the Object defines a "nil" (null)
///     type that is meant to represent a Lox nil, while the null literal means
///     a literal does not exist for the token.
///  * With lexeme is a slice, and currently is slicing right into the source code.
///     See scanner.Scanner.addToken()
pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    literal: ?Object,
    line: u32,

    pub fn init(self: *Token, token_type: TokenType, lexeme: []const u8, literal: ?Object, line: u32) void {
        self.token_type = token_type;
        self.lexeme = lexeme;
        self.literal = literal;
        self.line = line;
    }
};
