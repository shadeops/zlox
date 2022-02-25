const std = @import("std");

const Lox = @import("main.zig");
const Expr = @import("expr.zig");
const Stmt = @import("stmt.zig");

const Interpreter = @import("interpreter.zig").Interpreter;
const Token = @import("token.zig").Token;

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
    SUBCLASS,
};

var current_class = ClassType.NONE;

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

    fn exprInterface(self: *Self) Expr.VisitorInterface {
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

    fn stmtInterface(self: *Self) Stmt.VisitorInterface {
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

    fn visitAssignExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(expr.assign.value);
        try self.resolveLocal(expr, expr.assign.name);
    }

    fn visitBinaryExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(expr.binary.left);
        try self.resolve(expr.binary.right);
    }

    fn visitCallExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(expr.call.callee);

        for (expr.call.arguments.items) |argument| {
            try self.resolve(argument);
        }
    }

    fn visitGetExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(expr.get.object);
    }

    fn visitGroupingExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(expr.grouping.expression);
    }

    fn visitLiteralExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        _ = ptr;
        _ = expr;
    }

    fn visitLogicalExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(expr.logical.left);
        try self.resolve(expr.logical.right);
    }

    fn visitSetExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(expr.set.value);
        try self.resolve(expr.set.object);
    }

    fn visitSuperExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        if (current_class == .NONE) {
            Lox.tokenError(expr.super.keyword, "Can't use 'super' outside of a class.");
        } else if (current_class != .SUBCLASS) {
            Lox.tokenError(expr.super.keyword, "Can't use 'super' in a class with no superclass.");
        }
        try self.resolveLocal(expr, expr.super.keyword);
    }

    fn visitThisExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        if (current_class == .NONE) {
            Lox.tokenError(expr.this.keyword, "Can't use 'this' outside of a class.");
            return;
        }
        try self.resolveLocal(expr, expr.this.keyword);
    }

    fn visitUnaryExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(expr.unary.right);
    }

    fn visitVariableExpr(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);

        if (!self.scopeIsEmpty() and
            self.scopePeek().get(expr.variable.name.lexeme) != null and
            self.scopePeek().get(expr.variable.name.lexeme).? == false)
        {
            Lox.tokenError(expr.variable.name, "Can't read local variable in its own initializer.");
        }
        try self.resolveLocal(expr, expr.variable.name);
    }

    fn visitBlockStmt(ptr: *anyopaque, stmt: *const Stmt.Block) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.beginScope();
        try self.resolve(stmt.statements);
        self.endScope();
    }

    fn visitClassStmt(ptr: *anyopaque, stmt: *const Stmt.Class) anyerror!void {
        const self = castToSelf(Self, ptr);

        var enclosing_class = current_class;
        current_class = .CLASS;

        try self.declare(stmt.name);
        try self.define(stmt.name);

        if (stmt.superclass != null and
            std.mem.eql(u8, stmt.name.lexeme, stmt.superclass.?.name.lexeme))
        {
            Lox.tokenError(stmt.superclass.?.name, "A class can't inherit from itself.");
        }

        if (stmt.superclass != null) {
            current_class = .SUBCLASS;
            // might need to allocate this
            //var expr = @as(*const Expr.Expr, &Expr.Expr{.variable = stmt.superclass.?.*});
            var expr:*const Expr.Expr = Expr.Expr.create(self.allocator, stmt.superclass.?.*);
            try self.resolve(expr);
        }

        if (stmt.superclass != null) {
            self.beginScope();
            try self.scopePeek().put("super", true);
        }

        self.beginScope();
        try self.scopePeek().put("this", true);
        for (stmt.methods.items) |method| {
            var declaration = FunctionType.METHOD;
            if (std.mem.eql(u8, method.name.lexeme, "init")) {
                declaration = .INITIALIZER;
            }
            try self.resolveFunction(method, declaration);
        }
        self.endScope();

        if (stmt.superclass != null) {
            self.endScope();
        }

        current_class = enclosing_class;
    }

    fn visitExpressionStmt(ptr: *anyopaque, stmt: *const Stmt.Expression) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(stmt.expression);
    }

    fn visitFunctionStmt(ptr: *anyopaque, stmt: *const Stmt.Function) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.declare(stmt.name);
        try self.define(stmt.name);
        try self.resolveFunction(stmt, .FUNCTION);
    }

    fn visitIfStmt(ptr: *anyopaque, stmt: *const Stmt.If) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(stmt.condition);
        try self.resolve(stmt.then_branch);
        if (stmt.else_branch != null)
            try self.resolve(stmt.else_branch.?);
    }

    fn visitPrintStmt(ptr: *anyopaque, stmt: *const Stmt.Print) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(stmt.expression);
    }

    fn visitReturnStmt(ptr: *anyopaque, stmt: *const Stmt.Return) anyerror!void {
        const self = castToSelf(Self, ptr);
        if (self.current_function == .NONE) {
            Lox.tokenError(stmt.keyword, "Can't return from top-level code.");
        }

        if (stmt.value != null) {
            if (self.current_function == .INITIALIZER) {
                Lox.tokenError(stmt.keyword, "Can't return a value from an initializer.");
            }
            try self.resolve(stmt.value.?);
        }
    }

    fn visitVarStmt(ptr: *anyopaque, stmt: *const Stmt.Var) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.declare(stmt.name);
        if (stmt.initializer != null) {
            try self.resolve(stmt.initializer.?);
        }
        try self.define(stmt.name);
    }
    fn visitWhileStmt(ptr: *anyopaque, stmt: *const Stmt.While) anyerror!void {
        const self = castToSelf(Self, ptr);
        try self.resolve(stmt.condition);
        try self.resolve(stmt.body);
    }

    pub fn resolve(self: *Self, thing: anytype) !void {
        switch (@TypeOf(thing)) {
            Stmt.Stmt => {
                var iface = self.stmtInterface();
                try thing.accept(&iface);
            },
            *const Expr.Expr => {
                var iface = self.exprInterface();
                try thing.accept(&iface);
            },
            std.ArrayList(Stmt.Stmt) => {
                for (thing.items) |item| {
                    try self.resolve(item);
                }
            },
            else => {
                @compileError("resolve requires a Stmt.Stmt, " ++
                    "*const Expr.Expr or std.ArrayList(Stmt.Stmt)");
            },
        }
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
        try self.resolve(function.body);
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

    fn declare(self: *Self, name: *const Token) !void {
        if (self.scopeIsEmpty()) return;
        var scope = self.scopePeek();
        if (scope.contains(name.lexeme)) {
            Lox.tokenError(name, "Already a variable with this name in this scope.");
        }
        try scope.put(name.lexeme, false);
    }

    fn define(self: *Self, name: *const Token) !void {
        if (self.scopeIsEmpty()) return;
        var scope = self.scopePeek();
        try scope.put(name.lexeme, true);
    }

    fn resolveLocal(self: *Self, expr:*const Expr.Expr, name: *const Token) !void {
        for (self.scopes.items) |_, idx| {
            var i = (self.scopes.items.len - 1) - idx;
            if (self.scopes.items[i].contains(name.lexeme)) {
                try self.interpreter.resolve(expr, idx);
                return;
            }
        }
    }

    fn scopeIsEmpty(self: Self) bool {
        return self.scopes.items.len == 0;
    }

    fn scopePeek(self: Self) *std.StringHashMap(bool) {
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

//fn getExpr(x: *Expr.Literal) Expr.Expr {
//    var y = x.toExpr();
//    return y;
//}

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
