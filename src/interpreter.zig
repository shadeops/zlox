const std = @import("std");
const Token = @import("token.zig").Token;
const Object = @import("object.zig").Object;
const Environment = @import("environment.zig").Environment;
const Expr = @import("expr.zig");
const Stmt = @import("stmt.zig");

const runtimeError = @import("main.zig").runtimeError;

fn castToSelf(comptime T: type, ptr: *anyopaque) *T {
    const alignment = @alignOf(T);
    const self = @ptrCast(*T, @alignCast(alignment, ptr));
    return self;
}

pub const Interpreter = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    environment: Environment,

    // we capture the return value here instead of returning
    // a generic structure
    ret: ?Object = null,

    pub fn init(allocator: std.mem.Allocator) Self {
        return .{
            .allocator = allocator,
            .environment = Environment.init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.environment.deinit();
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

    pub fn interpret(self: *Self, statements: std.ArrayList(?Stmt.Stmt)) void {
        for (statements.items) |statement| {
            if (statement == null) {
                runtimeError("Invalid statement");
                continue;
            }
            self.execute(statement.?) catch |err| {
                runtimeError(@errorName(err));
            };
        }
    }

    fn visitAssignExpr(ptr: *anyopaque, expr: *const Expr.Assign) anyerror!void {
        _ = ptr;
        _ = expr;
    }

    fn visitBinaryExpr(ptr: *anyopaque, expr: *const Expr.Binary) anyerror!void {
        const self = castToSelf(Self, ptr);

        var left = try self.evaluate(expr.left);
        var right = try self.evaluate(expr.right);

        switch (expr.operator.token_type) {
            .GREATER => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initBoolean(left.?.value.number > right.?.value.number);
            },
            .GREATER_EQUAL => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initBoolean(left.?.value.number >= right.?.value.number);
            },
            .LESS => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initBoolean(left.?.value.number < right.?.value.number);
            },
            .LESS_EQUAL => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initBoolean(left.?.value.number <= right.?.value.number);
            },
            .MINUS => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initNumber(left.?.value.number - right.?.value.number);
            },
            .PLUS => {
                if (left == null or right == null) return error.OperandError;
                if (left.?.isType(.NUMBER) and right.?.isType(.NUMBER)) {
                    self.ret = Object.initNumber(left.?.value.number + right.?.value.number);
                    return;
                }
                if (left.?.isType(.STRING) and right.?.isType(.STRING)) {
                    var strs = [_][]const u8{ left.?.value.string, right.?.value.string };
                    var new_str = std.mem.concat(self.allocator, u8, &strs) catch unreachable;
                    self.ret = Object.initString(new_str);
                    return;
                }
                return error.OperandError;
                //std.info.err("Operand must be two numbers or two strings.", .{});
            },
            .SLASH => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initNumber(left.?.value.number / right.?.value.number);
            },
            .STAR => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initNumber(left.?.value.number * right.?.value.number);
            },
            .BANG_EQUAL => self.ret = Object.initBoolean(!isEqual(left, right)),
            .EQUAL_EQUAL => self.ret = Object.initBoolean(isEqual(left, right)),
            else => return,
        }
        return;
    }

    fn visitCallExpr(ptr: *anyopaque, expr: *const Expr.Call) anyerror!void {
        _ = ptr;
        _ = expr;
    }
    fn visitGetExpr(ptr: *anyopaque, expr: *const Expr.Get) anyerror!void {
        _ = ptr;
        _ = expr;
    }
    fn visitGroupingExpr(ptr: *anyopaque, expr: *const Expr.Grouping) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = try self.evaluate(expr.expression);
    }
    fn visitLiteralExpr(ptr: *anyopaque, expr: *const Expr.Literal) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = expr.value;
    }
    fn visitLogicalExpr(ptr: *anyopaque, expr: *const Expr.Logical) anyerror!void {
        _ = ptr;
        _ = expr;
    }
    fn visitSetExpr(ptr: *anyopaque, expr: *const Expr.Set) anyerror!void {
        _ = ptr;
        _ = expr;
    }
    fn visitSuperExpr(ptr: *anyopaque, expr: *const Expr.Super) anyerror!void {
        _ = ptr;
        _ = expr;
    }
    fn visitThisExpr(ptr: *anyopaque, expr: *const Expr.This) anyerror!void {
        _ = ptr;
        _ = expr;
    }
    fn visitUnaryExpr(ptr: *anyopaque, expr: *const Expr.Unary) anyerror!void {
        const self = castToSelf(Self, ptr);

        var right = try self.evaluate(expr.right);
        switch (expr.operator.token_type) {
            .BANG => {
                self.ret = Object.initBoolean(!isTruthy(right));
            },
            .MINUS => {
                try checkNumberOperand(expr.operator, right);
                self.ret = Object.initNumber(-right.?.value.number);
            },
            else => {
                self.ret = null;
            },
        }
    }
    fn visitVariableExpr(ptr: *anyopaque, expr: *const Expr.Variable) anyerror!void {
        const self = castToSelf(Self, ptr);

        self.ret = try self.environment.get(expr.name);
    }

    fn checkNumberOperand(operator: Token, operand: ?Object) !void {
        _ = operator;
        if (operand != null and operand.?.isType(.NUMBER)) return;
        //std.info.err("Operand must be a number.", .{});
        return error.OperandError;
    }

    fn checkNumberOperands(operator: Token, left: ?Object, right: ?Object) !void {
        _ = operator;
        if (left == null or right == null) return error.OperandError;
        if (left.?.isType(.NUMBER) and right.?.isType(.NUMBER)) return;
        //std.info.err("Operands must be a number.", .{});
        return error.OperandError;
    }

    fn evaluate(self: *Self, expr: Expr.Expr) !?Object {
        var iface = self.exprInterface();
        // Whichever Expr accepts the Interpreter Interface which will
        // update the Interpreter's ret value
        try expr.accept(&iface);
        return self.ret;
    }

    fn execute(self: *Self, stmt: Stmt.Stmt) !void {
        var iface = self.stmtInterface();
        try stmt.accept(&iface);
    }

    fn visitBlockStmt(ptr: *anyopaque, stmt: *const Stmt.Block) anyerror!void {
        _ = ptr;
        _ = stmt;
    }

    fn visitClassStmt(ptr: *anyopaque, stmt: *const Stmt.Class) anyerror!void {
        _ = ptr;
        _ = stmt;
    }

    fn visitExpressionStmt(ptr: *anyopaque, stmt: *const Stmt.Expression) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;
        _ = try self.evaluate(stmt.expression);
    }

    fn visitFunctionStmt(ptr: *anyopaque, stmt: *const Stmt.Function) anyerror!void {
        _ = ptr;
        _ = stmt;
    }

    fn visitIfStmt(ptr: *anyopaque, stmt: *const Stmt.If) anyerror!void {
        _ = ptr;
        _ = stmt;
    }

    fn visitPrintStmt(ptr: *anyopaque, stmt: *const Stmt.Print) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;
        var value = try self.evaluate(stmt.expression);
        self.stringify(value);
    }

    fn visitReturnStmt(ptr: *anyopaque, stmt: *const Stmt.Return) anyerror!void {
        _ = ptr;
        _ = stmt;
    }

    fn visitVarStmt(ptr: *anyopaque, stmt: *const Stmt.Var) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;
        
        var value: ?Object = null;
        if (stmt.initializer != null) {
            value = try self.evaluate(stmt.initializer.?);
        }

        self.environment.define(stmt.name.lexeme, value);
    }

    fn visitWhileStmt(ptr: *anyopaque, stmt: *const Stmt.While) anyerror!void {
        _ = ptr;
        _ = stmt;
    }

    fn isTruthy(object: ?Object) bool {
        if (object == null) return false;
        if (object.?.isType(.BOOLEAN)) return object.?.value.boolean;
        return true;
    }

    fn isEqual(a: ?Object, b: ?Object) bool {
        if (a == null and b == null) return true;
        if (a == null or b == null) return false;
        if (!a.?.isType(b.?.vtype)) return false;
        switch (a.?.vtype) {
            .NUMBER => return a.?.value.number == b.?.value.number,
            .BOOLEAN => return a.?.value.boolean == b.?.value.boolean,
            .STRING => return std.mem.eql(u8, a.?.value.string, b.?.value.string),
        }
        return false;
    }

    fn stringify(self: *Self, object: ?Object) void {
        const stdout = std.io.getStdOut();
        const writer = stdout.writer();
        if (object == null) {
            writer.print("nil\n", .{}) catch unreachable;
            return;
        }
        writer.print("{s}\n", .{object.?.toString(self.allocator)}) catch unreachable;
    }
};
