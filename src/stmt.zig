const std = @import("std");

const Expr = @import("expr.zig");
const Token = @import("token.zig").Token;

pub const VisitorInterface = struct {
    impl: *anyopaque,

    visitBlockStmtFn: fn (*anyopaque, *const Block) anyerror!void,
    visitClassStmtFn: fn (*anyopaque, *const Class) anyerror!void,
    visitExpressionStmtFn: fn (*anyopaque, *const Expression) anyerror!void,
    visitFunctionStmtFn: fn (*anyopaque, *const Function) anyerror!void,
    visitIfStmtFn: fn (*anyopaque, *const If) anyerror!void,
    visitPrintStmtFn: fn (*anyopaque, *const Print) anyerror!void,
    visitReturnStmtFn: fn (*anyopaque, *const Return) anyerror!void,
    visitVarStmtFn: fn (*anyopaque, *const Var) anyerror!void,
    visitWhileStmtFn: fn (*anyopaque, *const While) anyerror!void,

    pub fn visitBlockStmt(iface: *VisitorInterface, stmt: *const Block) anyerror!void {
        try iface.visitBlockStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitClassStmt(iface: *VisitorInterface, stmt: *const Class) anyerror!void {
        try iface.visitClassStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitExpressionStmt(iface: *VisitorInterface, stmt: *const Expression) anyerror!void {
        try iface.visitExpressionStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitFunctionStmt(iface: *VisitorInterface, stmt: *const Function) anyerror!void {
        try iface.visitFunctionStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitIfStmt(iface: *VisitorInterface, stmt: *const If) anyerror!void {
        try iface.visitIfStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitPrintStmt(iface: *VisitorInterface, stmt: *const Print) anyerror!void {
        try iface.visitPrintStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitReturnStmt(iface: *VisitorInterface, stmt: *const Return) anyerror!void {
        try iface.visitReturnStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitVarStmt(iface: *VisitorInterface, stmt: *const Var) anyerror!void {
        try iface.visitVarStmtFn(iface.impl, stmt);
        return;
    }
    pub fn visitWhileStmt(iface: *VisitorInterface, stmt: *const While) anyerror!void {
        try iface.visitWhileStmtFn(iface.impl, stmt);
        return;
    }
};

pub const Stmt = struct {
    impl: *const anyopaque,

    acceptFn: fn (*const anyopaque, *VisitorInterface) anyerror!void,

    pub fn accept(stmt: *const Stmt, visitor: *VisitorInterface) anyerror!void {
        try stmt.acceptFn(stmt.impl, visitor);
    }
};

fn castToConstSelf(comptime T: type, ptr: *const anyopaque) *const T {
    const alignment = @alignOf(T);
    const self = @ptrCast(*const T, @alignCast(alignment, ptr));
    return self;
}

pub const Block = struct {
    const Self = @This();
    statements: std.ArrayList(Stmt),

    pub fn init(statements: std.ArrayList(Stmt)) Self {
        return .{
            .statements = statements,
        };
    }

    pub fn create(allocator: std.mem.Allocator, statements: std.ArrayList(Stmt)) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(statements);
        return ptr;
    }

    pub fn toStmt(self: *const Self) Stmt {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitBlockStmt(self);
    }
};

pub const Class = struct {
    const Self = @This();
    name: Token,
    superclass: ?*Expr.Variable,
    methods: std.ArrayList(*const Function),

    pub fn init(
        name: Token,
        superclass: ?*Expr.Variable,
        methods: std.ArrayList(*const Function),
    ) Self {
        return .{
            .name = name,
            .superclass = superclass,
            .methods = methods,
        };
    }

    pub fn create(
        allocator: std.mem.Allocator,
        name: Token,
        superclass: ?*Expr.Variable,
        methods: std.ArrayList(*const Function),
    ) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(
            name,
            superclass,
            methods,
        );
        return ptr;
    }

    pub fn toStmt(self: *const Self) Stmt {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitClassStmt(self);
    }
};

pub const Expression = struct {
    const Self = @This();
    expression: Expr.Expr,

    pub fn init(expression: Expr.Expr) Self {
        return .{
            .expression = expression,
        };
    }

    pub fn create(
        allocator: std.mem.Allocator,
        expression: Expr.Expr,
    ) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(expression);
        return ptr;
    }

    pub fn toStmt(self: *const Self) Stmt {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitExpressionStmt(self);
    }
};

pub const Function = struct {
    const Self = @This();
    name: Token,
    params: std.ArrayList(Token),
    body: std.ArrayList(Stmt),

    pub fn init(name: Token, params: std.ArrayList(Token), body: std.ArrayList(Stmt)) Self {
        return .{
            .name = name,
            .params = params,
            .body = body,
        };
    }

    pub fn create(
        allocator: std.mem.Allocator,
        name: Token,
        params: std.ArrayList(Token),
        body: std.ArrayList(Stmt),
    ) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(name, params, body);
        return ptr;
    }

    pub fn toStmt(self: *const Self) Stmt {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitFunctionStmt(self);
    }
};

pub const If = struct {
    const Self = @This();
    condition: Expr.Expr,
    then_branch: Stmt,
    else_branch: ?Stmt,

    pub fn init(condition: Expr.Expr, then_branch: Stmt, else_branch: ?Stmt) Self {
        return .{
            .condition = condition,
            .then_branch = then_branch,
            .else_branch = else_branch,
        };
    }

    pub fn create(
        allocator: std.mem.Allocator,
        condition: Expr.Expr,
        then_branch: Stmt,
        else_branch: ?Stmt,
    ) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(condition, then_branch, else_branch);
        return ptr;
    }

    pub fn toStmt(self: *const Self) Stmt {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitIfStmt(self);
    }
};

pub const Print = struct {
    const Self = @This();
    expression: Expr.Expr,

    pub fn init(expression: Expr.Expr) Self {
        return .{
            .expression = expression,
        };
    }

    pub fn create(allocator: std.mem.Allocator, expression: Expr.Expr) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(expression);
        return ptr;
    }

    pub fn toStmt(self: *const Self) Stmt {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitPrintStmt(self);
    }
};

pub const Return = struct {
    const Self = @This();
    keyword: Token,
    value: ?Expr.Expr,

    pub fn init(keyword: Token, value: ?Expr.Expr) Self {
        return .{
            .keyword = keyword,
            .value = value,
        };
    }

    pub fn create(allocator: std.mem.Allocator, keyword: Token, value: ?Expr.Expr) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(keyword, value);
        return ptr;
    }

    pub fn toStmt(self: *const Self) Stmt {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitReturnStmt(self);
    }
};

pub const Var = struct {
    const Self = @This();
    name: Token,
    initializer: ?Expr.Expr,

    pub fn init(name: Token, initializer: ?Expr.Expr) Self {
        return .{
            .name = name,
            .initializer = initializer,
        };
    }

    pub fn create(
        allocator: std.mem.Allocator,
        name: Token,
        initializer: ?Expr.Expr,
    ) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(name, initializer);
        return ptr;
    }

    pub fn toStmt(self: *const Self) Stmt {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitVarStmt(self);
    }
};

pub const While = struct {
    const Self = @This();
    condition: Expr.Expr,
    body: Stmt,

    pub fn init(condition: Expr.Expr, body: Stmt) Self {
        return .{
            .condition = condition,
            .body = body,
        };
    }

    pub fn create(allocator: std.mem.Allocator, condition: Expr.Expr, body: Stmt) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(condition, body);
        return ptr;
    }

    pub fn toStmt(self: *const Self) Stmt {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitWhileStmt(self);
    }
};
