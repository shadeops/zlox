const std = @import("std");

const Expr = @import("expr.zig");
const Token = @import("token.zig").Token;

pub const VisitorInterface = struct {
    impl: *anyopaque,

    visitBlockStmtFn: fn (*anyopaque, *const Stmt) anyerror!void,
    visitClassStmtFn: fn (*anyopaque, *const Stmt) anyerror!void,
    visitExpressionStmtFn: fn (*anyopaque, *const Stmt) anyerror!void,
    visitFunctionStmtFn: fn (*anyopaque, *const Stmt) anyerror!void,
    visitIfStmtFn: fn (*anyopaque, *const Stmt) anyerror!void,
    visitPrintStmtFn: fn (*anyopaque, *const Stmt) anyerror!void,
    visitReturnStmtFn: fn (*anyopaque, *const Stmt) anyerror!void,
    visitVarStmtFn: fn (*anyopaque, *const Stmt) anyerror!void,
    visitWhileStmtFn: fn (*anyopaque, *const Stmt) anyerror!void,

    pub fn visitBlockStmt(iface: *VisitorInterface, stmt: *const Stmt) anyerror!void {
        try iface.visitBlockStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitClassStmt(iface: *VisitorInterface, stmt: *const Stmt) anyerror!void {
        try iface.visitClassStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitExpressionStmt(iface: *VisitorInterface, stmt: *const Stmt) anyerror!void {
        try iface.visitExpressionStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitFunctionStmt(iface: *VisitorInterface, stmt: *const Stmt) anyerror!void {
        try iface.visitFunctionStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitIfStmt(iface: *VisitorInterface, stmt: *const Stmt) anyerror!void {
        try iface.visitIfStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitPrintStmt(iface: *VisitorInterface, stmt: *const Stmt) anyerror!void {
        try iface.visitPrintStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitReturnStmt(iface: *VisitorInterface, stmt: *const Stmt) anyerror!void {
        try iface.visitReturnStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitVarStmt(iface: *VisitorInterface, stmt: *const Stmt) anyerror!void {
        try iface.visitVarStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitWhileStmt(iface: *VisitorInterface, stmt: *const Stmt) anyerror!void {
        try iface.visitWhileStmtFn(iface.impl, stmt);
        return;
    }
};

const StmtType = enum {
    block,
    class,
    expression,
    function,
    if_s,
    print,
    return_s,
    var_s,
    while_s,
};

pub const Stmt = union(StmtType) {
    const Self = @This();

    block: Block,
    class: Class,
    expression: Expression,
    function: Function,
    if_s: If,
    print: Print,
    return_s: Return,
    var_s: Var,
    while_s: While,

    pub fn accept(self: *const Self, visitor: *VisitorInterface) anyerror!void {
        switch (self.*) {
            .block => try Block.accept(self, visitor),
            .class => try Class.accept(self, visitor),
            .expression => try Expression.accept(self, visitor),
            .function => try Function.accept(self, visitor),
            .if_s => try If.accept(self, visitor),
            .print => try Print.accept(self, visitor),
            .return_s => try Return.accept(self, visitor),
            .var_s => try Var.accept(self, visitor),
            .while_s => try While.accept(self, visitor),
        }
    }

    pub fn create(allocator: std.mem.Allocator, any_stmt: anytype) *Self {
        var ptr = allocator.create(Self) catch unreachable;

        switch (@TypeOf(any_stmt)) {
            Block => ptr.* = .{ .block = any_stmt },
            Class => ptr.* = .{ .class = any_stmt },
            Expression => ptr.* = .{ .expression = any_stmt },
            Function => ptr.* = .{ .function = any_stmt },
            If => ptr.* = .{ .if_s = any_stmt },
            Print => ptr.* = .{ .print = any_stmt },
            Return => ptr.* = .{ .return_s = any_stmt },
            Var => ptr.* = .{ .var_s = any_stmt },
            While => ptr.* = .{ .while_s = any_stmt },
            else => @compileError("Not a valid Stmt Type"),
        }
        return ptr;
    }
};

pub const Block = struct {
    const Self = @This();
    statements: std.ArrayList(*const Stmt),

    pub fn init(statements: std.ArrayList(*const Stmt)) Self {
        return .{
            .statements = statements,
        };
    }

    fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitBlockStmt(stmt);
    }
};

pub const Class = struct {
    const Self = @This();
    name: *const Token,
    superclass: ?*const Expr.Expr,
    methods: std.ArrayList(*const Function),

    pub fn init(
        name: *const Token,
        superclass: ?*const Expr.Expr,
        methods: std.ArrayList(*const Function),
    ) Self {
        return .{
            .name = name,
            .superclass = superclass,
            .methods = methods,
        };
    }

    fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitClassStmt(stmt);
    }
};

pub const Expression = struct {
    const Self = @This();
    expression: *const Expr.Expr,

    pub fn init(expression: *const Expr.Expr) Self {
        return .{
            .expression = expression,
        };
    }

    fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitExpressionStmt(stmt);
    }
};

pub const Function = struct {
    const Self = @This();
    name: *const Token,
    params: std.ArrayList(*const Token),
    body: std.ArrayList(*const Stmt),

    pub fn init(
        name: *const Token,
        params: std.ArrayList(*const Token),
        body: std.ArrayList(*const Stmt),
    ) Self {
        return .{
            .name = name,
            .params = params,
            .body = body,
        };
    }

    fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitFunctionStmt(stmt);
    }
};

pub const If = struct {
    const Self = @This();
    condition: *const Expr.Expr,
    then_branch: *const Stmt,
    else_branch: ?*const Stmt,

    pub fn init(
        condition: *const Expr.Expr,
        then_branch: *const Stmt,
        else_branch: ?*const Stmt,
    ) Self {
        return .{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        };
    }

    fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitIfStmt(stmt);
    }
};

pub const Print = struct {
    const Self = @This();
    expression: *const Expr.Expr,

    pub fn init(expression: *const Expr.Expr) Self {
        return .{
            .expression = expression,
        };
    }

    fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitPrintStmt(stmt);
    }
};

pub const Return = struct {
    const Self = @This();
    keyword: *const Token,
    value: ?*const Expr.Expr,

    pub fn init(keyword: *const Token, value: ?*const Expr.Expr) Self {
        return .{
            .keyword = keyword,
            .value = value,
        };
    }

    fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitReturnStmt(stmt);
    }
};

pub const Var = struct {
    const Self = @This();
    name: *const Token,
    initializer: ?*const Expr.Expr,

    pub fn init(name: *const Token, initializer: ?*const Expr.Expr) Self {
        return .{
            .name = name,
            .initializer = initializer,
        };
    }

    fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitVarStmt(stmt);
    }
};

pub const While = struct {
    const Self = @This();
    condition: *const Expr.Expr,
    body: *const Stmt,

    pub fn init(condition: *const Expr.Expr, body: *const Stmt) Self {
        return .{
            .condition = condition,
            .body = body,
        };
    }

    fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitWhileStmt(stmt);
    }
};
