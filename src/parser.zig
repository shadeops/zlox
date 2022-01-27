const std = @import("std");
const Token = @import("token.zig").Token;
const TokenType = @import("token_types.zig").TokenType;
const Expr = @import("expr.zig");
const Object = @import("object.zig").Object;

const tokenError = @import("main.zig").tokenError;

const ParseError = error{
    Paren,
    Expression,
    OutOfMemory,
};

pub const Parser = struct {
    const Self = @This();
    tokens: std.ArrayList(Token),
    current: u32,
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator, tokens: std.ArrayList(Token)) Parser {
        return Parser{
            .tokens = tokens,
            .current = 0,
            .allocator = allocator,
        };
    }

    pub fn parse(self: *Self) ?Expr.Expr {
        return self.expression() catch {
            return null;
        };
    }

    fn expression(self: *Self) ParseError!Expr.Expr {
        return try self.equality();
    }

    fn equality(self: *Self) ParseError!Expr.Expr {
        var expr = try self.comparison();

        while (self.match(&.{ .BANG_EQUAL, .EQUAL_EQUAL })) {
            var operator = self.previous();
            var right = try self.comparison();
            expr = Expr.Binary.create(self.allocator, expr, operator, right).toExpr();
        }

        return expr;
    }

    fn comparison(self: *Self) ParseError!Expr.Expr {
        var expr = try self.term();

        while (self.match(&.{ .GREATER, .GREATER_EQUAL, .LESS, .LESS_EQUAL })) {
            var operator = self.previous();
            var right = try self.term();
            expr = Expr.Binary.create(self.allocator, expr, operator, right).toExpr();
        }

        return expr;
    }

    fn term(self: *Self) ParseError!Expr.Expr {
        var expr = try self.factor();

        while (self.match(&.{ .MINUS, .PLUS })) {
            var operator = self.previous();
            var right = try self.factor();
            expr = Expr.Binary.create(self.allocator, expr, operator, right).toExpr();
        }

        return expr;
    }

    fn factor(self: *Self) ParseError!Expr.Expr {
        var expr = try self.unary();

        while (self.match(&.{ .SLASH, .STAR })) {
            var operator = self.previous();
            var right = try self.unary();
            expr = Expr.Binary.create(self.allocator, expr, operator, right).toExpr();
        }

        return expr;
    }

    fn unary(self: *Self) ParseError!Expr.Expr {
        if (self.match(&.{ .BANG, .MINUS })) {
            var operator = self.previous();
            var right = try self.unary();
            return Expr.Unary.create(self.allocator, operator, right).toExpr();
        }

        return try self.primary();
    }

    fn primary(self: *Self) ParseError!Expr.Expr {
        if (self.match(&.{.FALSE}))
            return Expr.Literal.create(self.allocator, Object.initBoolean(false)).toExpr();
        if (self.match(&.{.TRUE}))
            return Expr.Literal.create(self.allocator, Object.initBoolean(true)).toExpr();
        if (self.match(&.{.NIL}))
            return Expr.Literal.create(self.allocator, null).toExpr();

        if (self.match(&.{ .NUMBER, .STRING }))
            return Expr.Literal.create(self.allocator, self.previous().literal.?).toExpr();

        if (self.match(&.{.LEFT_PAREN})) {
            var expr = try self.expression();
            _ = try self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
            return Expr.Grouping.create(self.allocator, expr).toExpr();
        }

        return ParseError.Expression;
    }

    fn match(self: *Self, token_types: []const TokenType) bool {
        for (token_types) |v| {
            if (self.check(v)) {
                _ = self.advance();
                return true;
            }
        }
        return false;
    }

    fn consume(self: *Self, token_type: TokenType, message: []const u8) ParseError!Token {
        if (self.check(token_type)) return self.advance();
        try reportError(self.peek(), message);
        return ParseError.Expression;
    }

    fn check(self: Self, token_type: TokenType) bool {
        if (self.isAtEnd()) return false;
        return self.peek().token_type == token_type;
    }

    fn advance(self: *Self) Token {
        if (!self.isAtEnd()) self.current += 1;
        return self.previous();
    }

    fn isAtEnd(self: Self) bool {
        return self.peek().token_type == .EOF;
    }

    fn peek(self: Self) Token {
        return self.tokens.items[self.current];
    }

    fn previous(self: Self) Token {
        return self.tokens.items[self.current - 1];
    }

    fn reportError(token: Token, message: []const u8) ParseError!void {
        tokenError(token, message) catch {
            // could not print
        };
        return error.Expression;
    }

    fn synchronize(self: *Self) void {
        self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().token_type == .SEMICOLON) return;

            switch (self.peek().token_type) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
            }
            self.advance();
        }
    }
};

test "Parser.check" {
    const a = std.testing.allocator;

    var tokens = std.ArrayList(Token).init(a);
    defer tokens.deinit();
    try tokens.append(.{ .token_type = .MINUS, .lexeme = "-", .literal = null, .line = 1 });
    try tokens.append(.{ .token_type = .PLUS, .lexeme = "+", .literal = null, .line = 1 });
    try tokens.append(.{ .token_type = .EOF, .lexeme = "", .literal = null, .line = 1 });

    var parser = Parser.init(a, tokens);
    try std.testing.expect(parser.check(.MINUS));
    try std.testing.expect(parser.advance().token_type == .MINUS);
    try std.testing.expect(parser.check(.PLUS));
    try std.testing.expect(parser.match(&.{.PLUS}));
    try std.testing.expect(parser.isAtEnd());
}
