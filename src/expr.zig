const std = @import("std");
const Token = @import("token.zig").Token;
const Object = @import("object.zig").Object;

pub const VisitorInterface = struct {
    impl: *anyopaque,

    visitExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitAssignExprFn: fn (*anyopaque, *const Assign) anyerror!void,
    visitBinaryExprFn: fn (*anyopaque, *const Binary) anyerror!void,
    visitCallExprFn: fn (*anyopaque, *const Call) anyerror!void,
    visitGetExprFn: fn (*anyopaque, *const Get) anyerror!void,
    visitGroupingExprFn: fn (*anyopaque, *const Grouping) anyerror!void,
    visitLiteralExprFn: fn (*anyopaque, *const Literal) anyerror!void,
    visitSetExprFn: fn (*anyopaque, *const Set) anyerror!void,
    visitSuperExprFn: fn (*anyopaque, *const Super) anyerror!void,
    visitThisExprFn: fn (*anyopaque, *const This) anyerror!void,
    visitUnaryExprFn: fn (*anyopaque, *const Unary) anyerror!void,
    visitVariableExprFn: fn (*anyopaque, *const Variable) anyerror!void,

    pub fn visitExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitExprFn(iface.impl, expr);
        return;
    }
    pub fn visitAssignExpr(iface: *VisitorInterface, expr: *const Assign) anyerror!void {
        iface.visitAssignExprFn(iface.impl, expr);
        return;
    }
    pub fn visitBinaryExpr(iface: *VisitorInterface, expr: *const Binary) anyerror!void {
        try iface.visitBinaryExprFn(iface.impl, expr);
        return;
    }
    pub fn visitCallExpr(iface: *VisitorInterface, expr: *const Call) anyerror!void {
        iface.visitCallExprFn(iface.impl, expr);
        return;
    }
    pub fn visitGetExpr(iface: *VisitorInterface, expr: *const Get) anyerror!void {
        iface.visitGetExprFn(iface.impl, expr);
        return;
    }
    pub fn visitGroupingExpr(iface: *VisitorInterface, expr: *const Grouping) anyerror!void {
        try iface.visitGroupingExprFn(iface.impl, expr);
        return;
    }
    pub fn visitLiteralExpr(iface: *VisitorInterface, expr: *const Literal) anyerror!void {
        try iface.visitLiteralExprFn(iface.impl, expr);
        return;
    }
    pub fn visitSetExpr(iface: *VisitorInterface, expr: *const Set) anyerror!void {
        iface.visitSetExprFn(iface.impl, expr);
        return;
    }
    pub fn visitSuperExpr(iface: *VisitorInterface, expr: *const Super) anyerror!void {
        iface.visitSuperExprFn(iface.impl, expr);
        return;
    }
    pub fn visitThisExpr(iface: *VisitorInterface, expr: *const This) anyerror!void {
        iface.visitThisExprFn(iface.impl, expr);
        return;
    }
    pub fn visitUnaryExpr(iface: *VisitorInterface, expr: *const Unary) anyerror!void {
        try iface.visitUnaryExprFn(iface.impl, expr);
        return;
    }
    pub fn visitVariableExpr(iface: *VisitorInterface, expr: *const Variable) anyerror!void {
        iface.visitVariableExprFn(iface.impl, expr);
        return;
    }
};

pub const Expr = struct {
    impl: *const anyopaque,

    acceptFn: fn (*const anyopaque, *VisitorInterface) anyerror!void,

    pub fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try expr.acceptFn(expr.impl, visitor);
    }
};

pub const Assign = struct {
    const Self = @This();
    name: Token,
    value: Expr,

    pub fn init(name: Token, value: Expr) Self {
        return .{
            .name = name,
            .value = value,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitAssignExpr(self);
    }
};

pub const Binary = struct {
    const Self = @This();
    left: Expr,
    operator: Token,
    right: Expr,

    pub fn init(left: Expr, operator: Token, right: Expr) Self {
        return .{
            .left = left,
            .operator = operator,
            .right = right,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitBinaryExpr(self);
    }
};

pub const Call = struct {
    const Self = @This();
    callee: Expr,
    paren: Token,
    arguments: std.ArrayList(Expr),

    pub fn init(callee: Expr, paren: Token, arguments: std.ArrayList(Expr)) Self {
        return .{
            .callee = callee,
            .paren = paren,
            .arguments = arguments,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitCallExpr(self);
    }
};

pub const Get = struct {
    const Self = @This();
    object: Expr,
    name: Token,

    pub fn init(object: Expr, name: Token) Self {
        return .{
            .object = object,
            .name = name,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitGetExpr(self);
    }
};

pub const Grouping = struct {
    const Self = @This();
    expression: Expr,

    pub fn init(expression: Expr) Self {
        return .{
            .expression = expression,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitGroupingExpr(self);
    }
};

pub const Literal = struct {
    const Self = @This();
    value: Token,

    pub fn init(value: Token) Self {
        return .{
            .value = value,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitLiteralExpr(self);
    }
};

pub const Logical = struct {
    const Self = @This();
    left: Expr,
    operator: Token,
    right: Expr,

    pub fn init(left: Expr, operator: Token, right: Expr) Self {
        return .{
            .left = left,
            .operator = operator,
            .right = right,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitLogicalExpr(self);
    }
};

pub const Set = struct {
    const Self = @This();
    object: Expr,
    name: Token,
    value: Expr,

    pub fn init(object: Expr, name: Token, value: Expr) Self {
        return .{
            .object = object,
            .name = name,
            .value = value,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitSetExpr(self);
    }
};

pub const Super = struct {
    const Self = @This();
    keyword: Token,
    method: Token,

    pub fn init(keyword: Token, method: Token) Self {
        return .{
            .keyword = keyword,
            .method = method,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitSuperExpr(self);
    }
};

pub const This = struct {
    const Self = @This();
    keyword: Token,

    pub fn init(keyword: Token) Self {
        return .{
            .keyword = keyword,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitThisExpr(self);
    }
};

pub const Unary = struct {
    const Self = @This();
    operator: Token,
    right: Expr,

    pub fn init(operator: Token, right: Expr) Self {
        return .{
            .operator = operator,
            .right = right,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitUnaryExpr(self);
    }
};

pub const Variable = struct {
    const Self = @This();
    name: Token,

    pub fn init(name: Token) Self {
        return .{
            .name = name,
        };
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const alignment = @alignOf(Self);
        const self = @ptrCast(*const Self, @alignCast(alignment, ptr));
        try visitor.visitVariableExpr(self);
    }
};
