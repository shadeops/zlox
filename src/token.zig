
const TokenType = @import("token_types.zig").TokenType;

pub const Literal = union {
    number: f64,
    string: []const u8,
};

pub const Token = struct {
    token_type: TokenType,
    lexeme: []const u8,
    literal: ?Literal,
    line: u32,

    pub fn init(self: *Token, token_type: TokenType, lexeme: []const u8, literal: Literal, line: u32) void {
        self.token_type = token_type;
        self.lexeme = lexeme;
        self.literal = literal;
        self.line = line;
    }

    pub fn toString(self: Token) []u8 {
        return self.lexeme;
    }
};
