const std = @import("std");
const line_error = @import("main.zig").line_error;

const Object = @import("object.zig").Object;
const Token = @import("token.zig").Token;
const TokenType = @import("token_types.zig").TokenType;

pub const Scanner = struct {
    source: []const u8,
    tokens: std.ArrayList(Token),
    start: u32,
    current: u32,
    line: u32,

    pub fn init(allocator: std.mem.Allocator, source: []const u8) !Scanner {
        return Scanner{
            .source = source,
            .tokens = std.ArrayList(Token).init(allocator),
            .start = 0,
            .current = 0,
            .line = 1,
        };
    }

    pub fn deinit(self: Scanner) void {
        self.tokens.deinit();
    }

    pub fn scanTokens(self: *Scanner) !std.ArrayList(Token) {
        while (!self.isAtEnd()) {
            self.start = self.current;
            try self.scanToken();
        }
        try self.tokens.append(.{
            .token_type = .EOF,
            .lexeme = "",
            .literal = null,
            .line = self.line,
        });
        return self.tokens;
    }

    fn isAtEnd(self: Scanner) bool {
        return self.current >= self.source.len;
    }

    fn advance(self: *Scanner) u8 {
        var previous = self.current;
        self.current += 1;
        return self.source[previous];
    }

    fn peek(self: Scanner) u8 {
        if (self.isAtEnd()) return 0;
        return self.source[self.current];
    }

    fn peekNext(self: Scanner) u8 {
        if (self.current + 1 >= self.source.len) return 0;
        return self.source[self.current + 1];
    }

    fn match(self: *Scanner, expected: u8) bool {
        if (self.isAtEnd()) return false;
        if (self.source[self.current] != expected) return false;
        self.current += 1;
        return true;
    }

    fn addToken(self: *Scanner, token_type: TokenType, literal: ?Object) !void {
        try self.tokens.append(.{
            .token_type = token_type,
            .lexeme = self.source[self.start..self.current],
            .literal = literal,
            .line = self.line,
        });
        return;
    }

    fn scanToken(self: *Scanner) !void {
        var c: u8 = self.advance();
        switch (c) {
            '(' => try self.addToken(.LEFT_PAREN, null),
            ')' => try self.addToken(.RIGHT_PAREN, null),
            '{' => try self.addToken(.LEFT_BRACE, null),
            '}' => try self.addToken(.RIGHT_BRACE, null),
            ',' => try self.addToken(.COMMA, null),
            '.' => try self.addToken(.DOT, null),
            '-' => try self.addToken(.MINUS, null),
            '+' => try self.addToken(.PLUS, null),
            ';' => try self.addToken(.SEMICOLON, null),
            '*' => try self.addToken(.STAR, null),
            '!' => try self.addToken(if (self.match('=')) .BANG_EQUAL else .BANG, null),
            '=' => try self.addToken(if (self.match('=')) .EQUAL_EQUAL else .EQUAL, null),
            '<' => try self.addToken(if (self.match('=')) .LESS_EQUAL else .LESS, null),
            '>' => try self.addToken(if (self.match('=')) .GREATER_EQUAL else .GREATER, null),
            '/' => if (self.match('/')) {
                while (self.peek() != '\n' and !self.isAtEnd()) _ = self.advance();
            } else {
                try self.addToken(.SLASH, null);
            },
            ' ', '\r', '\t', '\n' => self.line += 1,
            '"' => try self.string(),
            else => {
                if (isDigit(c)) {
                    try self.number();
                } else if (isAlpha(c)) {
                    try self.identifier();
                } else {
                    line_error(self.line, "Unexpected character.");
                }
            },
        }
        return;
    }

    fn string(self: *Scanner) !void {
        while (self.peek() != '"' and !self.isAtEnd()) {
            if (self.peek() == '\n') self.line += 1;
            _ = self.advance();
        }
        if (self.isAtEnd()) {
            line_error(self.line, "Unterminated string.");
            return;
        }
        _ = self.advance();
        var literal = Object.initString(self.source[self.start + 1 .. self.current - 1]);
        try self.addToken(.STRING, literal);
        return;
    }

    fn number(self: *Scanner) !void {
        while (isDigit(self.peek())) _ = self.advance();
        if (self.peek() == '.' and isDigit(self.peekNext())) _ = self.advance();
        while (isDigit(self.peek())) _ = self.advance();
        var num: f64 = std.fmt.parseFloat(f64, self.source[self.start..self.current]) catch {
            line_error(self.line, "Unable to parse float");
            return;
        };
        try self.addToken(.NUMBER, Object.initNumber(num));
    }

    fn identifier(self: *Scanner) !void {
        while (isAlphaNumeric(self.peek())) _ = self.advance();
        var identifier_token: TokenType = matchIdentifier(self.source[self.start..self.current]) orelse .IDENTIFIER;
        try self.addToken(identifier_token, null);
        return;
    }
};

fn matchIdentifier(str: []const u8) ?TokenType {
    if (std.mem.eql(u8, "and", str)) return .AND;
    if (std.mem.eql(u8, "class", str)) return .CLASS;
    if (std.mem.eql(u8, "else", str)) return .ELSE;
    if (std.mem.eql(u8, "false", str)) return .FALSE;
    if (std.mem.eql(u8, "for", str)) return .FOR;
    if (std.mem.eql(u8, "fun", str)) return .FUN;
    if (std.mem.eql(u8, "if", str)) return .IF;
    if (std.mem.eql(u8, "nil", str)) return .NIL;
    if (std.mem.eql(u8, "or", str)) return .OR;
    if (std.mem.eql(u8, "print", str)) return .PRINT;
    if (std.mem.eql(u8, "return", str)) return .RETURN;
    if (std.mem.eql(u8, "super", str)) return .SUPER;
    if (std.mem.eql(u8, "this", str)) return .THIS;
    if (std.mem.eql(u8, "var", str)) return .VAR;
    if (std.mem.eql(u8, "while", str)) return .WHILE;
    return null;
}

fn isDigit(c: u8) bool {
    return c >= '0' and c <= '9';
}

fn isAlpha(c: u8) bool {
    return (c >= 'a' and c <= 'z') or (c >= 'A' and c <= 'Z') or c == '_';
}

fn isAlphaNumeric(c: u8) bool {
    return isDigit(c) or isAlpha(c);
}
