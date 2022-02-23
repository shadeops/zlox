const std = @import("std");

const Lox = @import("main.zig");
const Expr = @import("expr.zig");
const Stmt = @import("stmt.zig");

const Object = @import("object.zig").Object;
const Token = @import("token.zig").Token;
const TokenType = @import("token_types.zig").TokenType;

pub const ParseError = error{
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

    pub fn parse(self: *Self) ParseError!std.ArrayList(Stmt.Stmt) {
        var statements = std.ArrayList(Stmt.Stmt).init(self.allocator);
        while (!self.isAtEnd()) {
            // NOTE:
            //  * In jlox we just append to the statements if it is a null
            //      or Stmt. For this to avoid having an ArrayList with an optional
            //      we'll just ignore the null.
            var decl = self.declaration();
            if (decl != null)
                try statements.append(decl.?);
        }
        return statements;
    }

    fn expression(self: *Self) ParseError!Expr.Expr {
        return try self.assignment();
    }

    // In Jlox declaration() wraps this block up in a try/catch
    // and if there is an error do a synchronize(). Since we can't
    // do block try/catch and to avoid having to add a catch to each
    // function call, we'll just wrap the entire function.
    fn declarationWithErrors(self: *Self) ParseError!?Stmt.Stmt {
        if (self.match(&.{.CLASS})) {
            return try self.classDeclaration();
        }
        if (self.match(&.{.FUN})) {
            return try self.function("function");
        }
        if (self.match(&.{.VAR})) {
            return try self.varDeclaration();
        }
        return try self.statement();
    }

    fn declaration(self: *Self) ?Stmt.Stmt {
        return self.declarationWithErrors() catch {
            self.synchronize();
            return null;
        };
    }

    fn classDeclaration(self: *Self) ParseError!Stmt.Stmt {
        var name = try self.consume(.IDENTIFIER, "Expect class name.");

        var superclass: ?*Expr.Variable = null;
        if (self.match(&.{.LESS})) {
            _ = try self.consume(.IDENTIFIER, "Expect superclass name.");
            superclass = Expr.Variable.create(self.allocator, self.previous());
        }

        _ = try self.consume(.LEFT_BRACE, "Expect '{' before class body.");

        var methods = std.ArrayList(*const Stmt.Function).init(self.allocator);
        while (!self.check(.RIGHT_BRACE) and !self.isAtEnd()) {
            var func_stmt = try self.function("method");
            const alignment = @alignOf(Stmt.Function);
            var func = @ptrCast(*const Stmt.Function, @alignCast(alignment, func_stmt.impl));
            try methods.append(func);
        }
        _ = try self.consume(.RIGHT_BRACE, "Expect '}' after class body.");
        return Stmt.Class.create(self.allocator, name, superclass, methods).toStmt();
    }

    fn statement(self: *Self) ParseError!Stmt.Stmt {
        if (self.match(&.{.FOR})) return self.forStatement();
        if (self.match(&.{.IF})) return self.ifStatement();
        if (self.match(&.{.PRINT})) return self.printStatement();
        if (self.match(&.{.RETURN})) return self.returnStatement();
        if (self.match(&.{.WHILE})) return self.whileStatement();
        if (self.match(&.{.LEFT_BRACE}))
            return Stmt.Block.create(self.allocator, try self.block()).toStmt();

        return self.expressionStatement();
    }

    fn forStatement(self: *Self) ParseError!Stmt.Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expect '(' after 'for'.");

        var initializer: ?Stmt.Stmt = null;
        if (self.match(&.{.SEMICOLON})) {
            initializer = null;
        } else if (self.match(&.{.VAR})) {
            initializer = try self.varDeclaration();
        } else {
            initializer = try self.expressionStatement();
        }

        var condition: ?Expr.Expr = null;
        if (!self.check(.SEMICOLON)) {
            condition = try self.expression();
        }

        _ = try self.consume(.SEMICOLON, "Expect ';' after loop condition.");

        var increment: ?Expr.Expr = null;
        if (!self.check(.RIGHT_PAREN)) {
            increment = try self.expression();
        }

        _ = try self.consume(.RIGHT_PAREN, "Expect ')' after flor clauses.");
        var body = try self.statement();

        if (increment != null) {
            var list = std.ArrayList(Stmt.Stmt).init(self.allocator);
            try list.append(body);
            try list.append(Stmt.Expression.create(self.allocator, increment.?).toStmt());
            body = Stmt.Block.create(self.allocator, list).toStmt();
        }

        if (condition == null)
            condition = Expr.Literal.create(self.allocator, Object.initBoolean(true)).toExpr();
        body = Stmt.While.create(self.allocator, condition.?, body).toStmt();

        if (initializer != null) {
            var list = std.ArrayList(Stmt.Stmt).init(self.allocator);
            try list.append(initializer.?);
            try list.append(body);
            body = Stmt.Block.create(self.allocator, list).toStmt();
        }

        return body;
    }

    fn ifStatement(self: *Self) ParseError!Stmt.Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expect '(' after 'if'.");
        var condition = try self.expression();
        _ = try self.consume(.RIGHT_PAREN, "Expect ')' after 'if'.");

        var then_branch = try self.statement();
        var else_branch: ?Stmt.Stmt = null;

        if (self.match(&.{.ELSE})) {
            else_branch = try self.statement();
        }

        return Stmt.If.create(self.allocator, condition, then_branch, else_branch).toStmt();
    }

    fn printStatement(self: *Self) ParseError!Stmt.Stmt {
        var value = try self.expression();
        _ = try self.consume(.SEMICOLON, "Expect ';' after value.");
        return Stmt.Print.create(self.allocator, value).toStmt();
    }

    fn returnStatement(self: *Self) ParseError!Stmt.Stmt {
        var keyword = self.previous();
        var value: ?Expr.Expr = null;
        if (!self.check(.SEMICOLON)) {
            value = try self.expression();
        }
        _ = try self.consume(.SEMICOLON, "Expect ';' after return value.");
        return Stmt.Return.create(self.allocator, keyword, value).toStmt();
    }

    fn varDeclaration(self: *Self) ParseError!Stmt.Stmt {
        var name = try self.consume(.IDENTIFIER, "Expect variable name.");

        var initializer: ?Expr.Expr = null;
        if (self.match(&.{.EQUAL})) {
            initializer = try self.expression();
        }
        _ = try self.consume(.SEMICOLON, "Expect ';' after variable declaration");
        return Stmt.Var.create(self.allocator, name, initializer).toStmt();
    }

    fn whileStatement(self: *Self) ParseError!Stmt.Stmt {
        _ = try self.consume(.LEFT_PAREN, "Expect '(' after 'while'.");
        var condition = try self.expression();
        _ = try self.consume(.RIGHT_PAREN, "Expect ')' after 'condition'.");
        var body = try self.statement();

        return Stmt.While.create(self.allocator, condition, body).toStmt();
    }

    fn expressionStatement(self: *Self) ParseError!Stmt.Stmt {
        var expr = try self.expression();
        _ = try self.consume(.SEMICOLON, "Expect ';' after expression.");
        return Stmt.Expression.create(self.allocator, expr).toStmt();
    }

    fn function(self: *Self, kind: []const u8) ParseError!Stmt.Stmt {

        // limit function name to 128 chars to avoid allocation
        var buf: [256]u8 = undefined;
        var kind_len = @minimum(kind.len, 128);
        var result: []const u8 = undefined;

        result = std.fmt.bufPrint(buf[0..], "Expect {s} name.", .{kind[0..kind_len]}) catch unreachable;
        var name = try self.consume(.IDENTIFIER, result);

        result = std.fmt.bufPrint(buf[0..], "Expect '(' after {s} name.", .{kind[0..kind_len]}) catch unreachable;
        _ = try self.consume(.LEFT_PAREN, result);

        var parameters = std.ArrayList(Token).init(self.allocator);
        if (!self.check(.RIGHT_PAREN)) {
            try parameters.append(try self.consume(.IDENTIFIER, "Expect parameter name."));
            while (self.match(&.{.COMMA})) {
                if (parameters.items.len >= 255) {
                    try reportError(self.peek(), "Can't have more than 255 parameters.");
                }
                try parameters.append(try self.consume(.IDENTIFIER, "Expect parameter name."));
            }
        }
        _ = try self.consume(.RIGHT_PAREN, "Expect ')' after parameters.");

        result = std.fmt.bufPrint(
            buf[0..],
            "Expect '{{' before {s} body.",
            .{kind[0..kind_len]},
        ) catch unreachable;
        _ = try self.consume(.LEFT_BRACE, result);
        var body = try self.block();
        return Stmt.Function.create(self.allocator, name, parameters, body).toStmt();
    }

    fn block(self: *Self) ParseError!std.ArrayList(Stmt.Stmt) {
        var statements = std.ArrayList(Stmt.Stmt).init(self.allocator);
        while (!self.check(.RIGHT_BRACE) and !self.isAtEnd()) {
            // if we hit a null skip over it.
            try statements.append(self.declaration() orelse continue);
        }
        _ = try self.consume(.RIGHT_BRACE, "Expect '}' after block.");
        return statements;
    }

    fn assignment(self: *Self) ParseError!Expr.Expr {
        var expr = try self.orOp();

        if (self.match(&.{.EQUAL})) {
            var equals = self.previous();
            var value = try self.assignment();

            if (expr.expr_type == .VARIABLE) {
                const alignment = @alignOf(Expr.Variable);
                var variable_expr = @ptrCast(
                    *const Expr.Variable,
                    @alignCast(alignment, expr.impl),
                );
                var name = variable_expr.name;
                return Expr.Assign.create(self.allocator, name, value).toExpr();
            } else if (expr.expr_type == .GET) {
                const alignment = @alignOf(Expr.Get);
                var get = @ptrCast(*const Expr.Get, @alignCast(alignment, expr.impl));
                return Expr.Set.create(self.allocator, get.object, get.name, value).toExpr();
            }

            try reportError(equals, "Invalid assignment target.");
        }

        return expr;
    }

    fn orOp(self: *Self) ParseError!Expr.Expr {
        var expr = try self.andOp();

        while (self.match(&.{.OR})) {
            var operator = self.previous();
            var right = try self.andOp();
            expr = Expr.Logical.create(self.allocator, expr, operator, right).toExpr();
        }

        return expr;
    }

    fn andOp(self: *Self) ParseError!Expr.Expr {
        var expr = try self.equality();

        while (self.match(&.{.AND})) {
            var operator = self.previous();
            var right = try self.equality();
            expr = Expr.Logical.create(self.allocator, expr, operator, right).toExpr();
        }

        return expr;
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

        return try self.call();
    }

    fn finishCall(self: *Self, callee: Expr.Expr) ParseError!Expr.Expr {
        var arguments = std.ArrayList(Expr.Expr).init(self.allocator);
        if (!self.check(.RIGHT_PAREN)) {
            try arguments.append(try self.expression());
            while (self.match(&.{.COMMA})) {
                if (arguments.items.len >= 255) {
                    try reportError(self.peek(), "Can't have more than 255 arguments.");
                }
                try arguments.append(try self.expression());
            }
        }

        var paren = try self.consume(.RIGHT_PAREN, "Expect ')' after arguments.");
        return Expr.Call.create(self.allocator, callee, paren, arguments).toExpr();
    }

    fn call(self: *Self) ParseError!Expr.Expr {
        var expr = try self.primary();

        while (true) {
            if (self.match(&.{.LEFT_PAREN})) {
                expr = try self.finishCall(expr);
            } else if (self.match(&.{.DOT})) {
                var name = try self.consume(.IDENTIFIER, "Expect property name after '.'.");
                expr = Expr.Get.create(self.allocator, expr, name).toExpr();
            } else {
                break;
            }
        }

        return expr;
    }

    fn primary(self: *Self) ParseError!Expr.Expr {
        if (self.match(&.{.FALSE}))
            return Expr.Literal.create(self.allocator, Object.initBoolean(false)).toExpr();
        if (self.match(&.{.TRUE}))
            return Expr.Literal.create(self.allocator, Object.initBoolean(true)).toExpr();
        if (self.match(&.{.NIL}))
            return Expr.Literal.create(self.allocator, Object.initNil()).toExpr();
        if (self.match(&.{ .NUMBER, .STRING }))
            return Expr.Literal.create(self.allocator, self.previous().literal.?).toExpr();
        if (self.match(&.{.SUPER})) {
            var keyword = self.previous();
            _ = try self.consume(.DOT, "Expect '.' after 'super'.");
            var method = try self.consume(.IDENTIFIER, "Expect superclass method name.");
            return Expr.Super.create(self.allocator, keyword, method).toExpr();
        }
        if (self.match(&.{.THIS}))
            return Expr.This.create(self.allocator, self.previous()).toExpr();
        if (self.match(&.{.IDENTIFIER}))
            return Expr.Variable.create(self.allocator, self.previous()).toExpr();
        if (self.match(&.{.LEFT_PAREN})) {
            var expr = try self.expression();
            _ = try self.consume(.RIGHT_PAREN, "Expect ')' after expression.");
            return Expr.Grouping.create(self.allocator, expr).toExpr();
        }

        try reportError(self.peek(), "Expect expression.");
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
        Lox.tokenError(token, message);
        return ParseError.Expression;
    }

    fn synchronize(self: *Self) void {
        _ = self.advance();

        while (!self.isAtEnd()) {
            if (self.previous().token_type == .SEMICOLON) return;

            switch (self.peek().token_type) {
                .CLASS, .FUN, .VAR, .FOR, .IF, .WHILE, .PRINT, .RETURN => return,
                else => {},
            }
            _ = self.advance();
        }
    }
};

test "Parser.check" {
    const a = std.testing.allocator;

    var tokens = std.ArrayList(Token).init(a);
    defer tokens.deinit();
    try tokens.append(.{
        .token_type = .MINUS,
        .lexeme = "-",
        .literal = Object.initNil(),
        .line = 1,
    });
    try tokens.append(.{
        .token_type = .PLUS,
        .lexeme = "+",
        .literal = Object.initNil(),
        .line = 1,
    });
    try tokens.append(.{
        .token_type = .EOF,
        .lexeme = "",
        .literal = Object.initNil(),
        .line = 1,
    });

    var parser = Parser.init(a, tokens);
    try std.testing.expect(parser.check(.MINUS));
    try std.testing.expect(parser.advance().token_type == .MINUS);
    try std.testing.expect(parser.check(.PLUS));
    try std.testing.expect(parser.match(&.{.PLUS}));
    try std.testing.expect(parser.isAtEnd());
}
