const std = @import("std");
const Token = @import("token.zig").Token;
const Expr = @import("expr.zig");
const Stmt = @import("stmt.zig");
const Interpreter = @import("interpreter.zig").Interpreter;

const tokenError = @import("main.zig").tokenError;

fn castToSelf(comptime T: type, ptr: *anyopaque) *T {
    const alignment = @alignOf(T);
    const self = @ptrCast(*T, @alignCast(alignment, ptr));
    return self;
}

const FunctionType = enum {
    NONE,
    FUNCTION,
    INITIALIZER,
    METHOD,
};

const ClassType = enum {
    NONE,
    CLASS,
};

var currentClass = ClassType.NONE;

pub const Resolver = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    interpreter: *Interpreter,
    scopes: std.ArrayList(*std.StringHashMap(bool)),
    current_function: FunctionType,

    pub fn init(allocator: std.mem.Allocator, interpreter: *Interpreter) Self {
        return .{
            .allocator = allocator,
            .interpreter = interpreter,
            .scopes = std.ArrayList(*std.StringHashMap(bool)).init(allocator),
            .current_function = .NONE,
        };
    }

    pub fn deinit(self: *Self) void {
        self.scopes.deinit();
        // We don't need to destroy/deinit the hash maps as that is done in the
        // self.endScope()
        if (self.scopes.items.len != 0) unreachable;
    }

    pub fn exprInterface(self: *Self) Expr.VisitorInterface {
        return .{
            .impl = @ptrCast(*anyopaque, self),
            .visitAssignExprFn = visitAssignExpr,
            .visitBinaryExprFn = visitBinaryExpr,
            .visitCallExprFn = visitCallExpr,
            .visitGetExprFn = visitGetExpr,
            .visitGroupingExprFn = visitGroupingExpr,
            .visitLiteralExprFn = visitLiteralExpr,
            .visitLogicalExprFn = visitLogicalExpr,
            .visitSetExprFn = visitSetExpr,
            .visitSuperExprFn = visitSuperExpr,
            .visitThisExprFn = visitThisExpr,
            .visitUnaryExprFn = visitUnaryExpr,
            .visitVariableExprFn = visitVariableExpr,
        };
    }

    pub fn stmtInterface(self: *Self) Stmt.VisitorInterface {
        return .{
            .impl = @ptrCast(*anyopaque, self),
            .visitBlockStmtFn = visitBlockStmt,
            .visitClassStmtFn = visitClassStmt,
            .visitExpressionStmtFn = visitExpressionStmt,
            .visitFunctionStmtFn = visitFunctionStmt,
            .visitIfStmtFn = visitIfStmt,
            .visitPrintStmtFn = visitPrintStmt,
            .visitReturnStmtFn = visitReturnStmt,
            .visitVarStmtFn = visitVarStmt,
            .visitWhileStmtFn = visitWhileStmt,
        };
    }

    fn visitAssignExpr(ptr: *anyopaque, expr: *const Expr.Assign) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(expr.value);
        try self.resolveLocal(expr.toExpr(), expr.name);
    }

    fn visitBinaryExpr(ptr: *anyopaque, expr: *const Expr.Binary) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(expr.left);
        try self.resolveExpr(expr.right);
    }

    fn visitCallExpr(ptr: *anyopaque, expr: *const Expr.Call) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(expr.callee);

        for (expr.arguments.items) |argument| {
            try self.resolveExpr(argument);
        }
    }

    fn visitGetExpr(ptr: *anyopaque, expr: *const Expr.Get) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(expr.object);
    }

    fn visitGroupingExpr(ptr: *anyopaque, expr: *const Expr.Grouping) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(expr.expression);
    }

    fn visitLiteralExpr(ptr: *anyopaque, expr: *const Expr.Literal) anyerror!void {
        _ = ptr;
        _ = expr;
    }

    fn visitLogicalExpr(ptr: *anyopaque, expr: *const Expr.Logical) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(expr.left);
        try self.resolveExpr(expr.right);
    }

    fn visitSetExpr(ptr: *anyopaque, expr: *const Expr.Set) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(expr.value);
        try self.resolveExpr(expr.object);
    }

    fn visitSuperExpr(ptr: *anyopaque, expr: *const Expr.Super) anyerror!void {
        _ = ptr;
        _ = expr;
    }

    fn visitThisExpr(ptr: *anyopaque, expr: *const Expr.This) anyerror!void {
        const self = castToSelf(Self, ptr);
        if (currentClass == .NONE) {
            try tokenError(expr.keyword, "Can't use 'this' outside of class.");
            return;
        }
        try self.resolveLocal(expr.toExpr(), expr.keyword);
    }

    fn visitUnaryExpr(ptr: *anyopaque, expr: *const Expr.Unary) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(expr.right);
    }

    fn visitVariableExpr(ptr: *anyopaque, expr: *const Expr.Variable) anyerror!void {
        const self = castToSelf(Self, ptr);

        if (!self.isEmpty() and
            self.peek().get(expr.name.lexeme) != null and
            self.peek().get(expr.name.lexeme).? == false)
        {
            try tokenError(expr.name, "Can't read local variable in its own initializer.");
        }
        try self.resolveLocal(expr.toExpr(), expr.name);
    }

    fn visitBlockStmt(ptr: *anyopaque, stmt: *const Stmt.Block) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.beginScope();
        try self.resolveStmts(stmt.statements);
        self.endScope();
    }

    fn visitClassStmt(ptr: *anyopaque, stmt: *const Stmt.Class) anyerror!void {
        const self = castToSelf(Self, ptr);

        var enclosingClass = currentClass;
        defer currentClass = enclosingClass;
        currentClass = .CLASS;

        try self.declare(stmt.name);
        try self.define(stmt.name);

        self.beginScope();
        try self.peek().put("this", true);
        for (stmt.methods.items) |method| {
            var declaration = FunctionType.METHOD;
            if (std.mem.eql(u8, method.name.lexeme, "init")) {
                declaration = .INITIALIZER;
            }
            try self.resolveFunction(method, declaration);
        }
        self.endScope();
    }

    fn visitExpressionStmt(ptr: *anyopaque, stmt: *const Stmt.Expression) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(stmt.expression);
    }

    fn visitFunctionStmt(ptr: *anyopaque, stmt: *const Stmt.Function) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.declare(stmt.name);
        try self.define(stmt.name);
        try self.resolveFunction(stmt, .FUNCTION);
    }

    fn visitIfStmt(ptr: *anyopaque, stmt: *const Stmt.If) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(stmt.condition);
        try self.resolveStmt(stmt.then_branch);
        if (stmt.else_branch != null)
            try self.resolveStmt(stmt.else_branch.?);
    }

    fn visitPrintStmt(ptr: *anyopaque, stmt: *const Stmt.Print) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(stmt.expression);
    }

    fn visitReturnStmt(ptr: *anyopaque, stmt: *const Stmt.Return) anyerror!void {
        const self = castToSelf(Self, ptr);
        if (self.current_function == .NONE) {
            try tokenError(stmt.keyword, "Can't return from top-level code.");
        }

        if (stmt.value != null) {
            if (self.current_function == .INITIALIZER) {
                try tokenError(stmt.keyword, "Can't return a value from an initializer.");
            }
            try self.resolveExpr(stmt.value.?);
        }
    }

    fn visitVarStmt(ptr: *anyopaque, stmt: *const Stmt.Var) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.declare(stmt.name);
        if (stmt.initializer != null) {
            try self.resolveExpr(stmt.initializer.?);
        }
        try self.define(stmt.name);
    }
    fn visitWhileStmt(ptr: *anyopaque, stmt: *const Stmt.While) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolveExpr(stmt.condition);
        try self.resolveStmt(stmt.body);
    }

    // TODO create a single resolve figures out which of the 3 to call
    // via comptime expression
    pub fn resolveStmts(self: *Self, statements: std.ArrayList(Stmt.Stmt)) !void {
        for (statements.items) |statement| {
            try self.resolveStmt(statement);
        }
    }

    fn resolveStmt(self: *Self, statement: Stmt.Stmt) !void {
        var iface = self.stmtInterface();
        try statement.accept(&iface);
    }

    fn resolveExpr(self: *Self, expr: Expr.Expr) !void {
        var iface = self.exprInterface();
        try expr.accept(&iface);
    }

    fn resolveFunction(
        self: *Self,
        function: *const Stmt.Function,
        function_type: FunctionType,
    ) !void {
        var enclosing_function = self.current_function;
        self.current_function = function_type;
        self.beginScope();
        for (function.params.items) |param| {
            try self.declare(param);
            try self.define(param);
        }
        try self.resolveStmts(function.body);
        self.endScope();
        self.current_function = enclosing_function;
    }

    fn beginScope(self: *Self) void {
        var map = self.allocator.create(std.StringHashMap(bool)) catch unreachable;
        map.* = std.StringHashMap(bool).init(self.allocator);
        self.scopes.append(map) catch {
            std.log.err("Could not append to resolver scope", .{});
        };
    }

    fn endScope(self: *Self) void {
        var scope = self.scopes.pop();
        scope.deinit();
        self.allocator.destroy(scope);
    }

    fn declare(self: *Self, name: Token) !void {
        if (self.isEmpty()) return;
        var scope = self.peek();
        if (scope.contains(name.lexeme)) {
            try tokenError(name, "Already a varaible with this name in this scope.");
        }
        try scope.put(name.lexeme, false);
    }

    fn define(self: *Self, name: Token) !void {
        if (self.isEmpty()) return;
        var scope = self.peek();
        try scope.put(name.lexeme, true);
    }

    fn resolveLocal(self: *Self, expr: Expr.Expr, name: Token) !void {
        for (self.scopes.items) |_, idx| {
            var i = (self.scopes.items.len - 1) - idx;
            if (self.scopes.items[i].contains(name.lexeme)) {
                try self.interpreter.resolve(expr, idx);
                return;
            }
        }
    }

    fn isEmpty(self: Self) bool {
        return self.scopes.items.len == 0;
    }

    fn peek(self: Self) *std.StringHashMap(bool) {
        return self.scopes.items[self.scopes.items.len - 1];
    }
};

test "Resolver.maptest" {
    //    var stack =  std.ArrayList(std.StringHashMap(bool)).init(std.testing.allocator);
    //    defer stack.deinit();
    //    try stack.append(std.StringHashMap(bool).init(std.testing.allocator));
    //    defer {
    //        for (stack.items) |*map| {
    //            map.deinit();
    //        }
    //    }
    //    try stack.items[0].put("a", true);
    //    try std.testing.expect(stack.items[0].get("a").?);
    //    var a = stack.items[0];
    //    try std.testing.expect(a.get("a").?);
    //    try a.put("b", true);
    //    try std.testing.expect(a.get("b").?);
    //    try std.testing.expect(stack.items[0].get("b").?);
}

fn getExpr(x: *Expr.Literal) Expr.Expr {
    var y = x.toExpr();
    return y;
}

test "Resolve.ptrmap" {
    //    const Object = @import("object.zig").Object;
    //    var literal_a = Expr.Literal.create(std.testing.allocator, Object.initNumber(1));
    //    defer std.testing.allocator.destroy(literal_a);
    //    var expr_a = literal_a.toExpr();
    //    var literal_b = Expr.Literal.create(std.testing.allocator, Object.initNumber(2));
    //    defer std.testing.allocator.destroy(literal_b);
    //    var expr_b = literal_b.toExpr();
    //    var expr_c = getExpr(literal_a);
    //    var map = std.AutoHashMap(Expr.Expr, usize).init(std.testing.allocator);
    //    defer map.deinit();
    //    try map.put(expr_a, 1);
    //    try map.put(expr_b, 2);
    //    try std.testing.expect(map.get(expr_c).? == 1);
}
