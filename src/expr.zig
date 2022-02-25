const std = @import("std");
const Token = @import("token.zig").Token;
const Object = @import("object.zig").Object;

pub const VisitorInterface = struct {
    impl: *anyopaque,

    visitAssignExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitBinaryExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitCallExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitGetExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitGroupingExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitLiteralExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitLogicalExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitSetExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitSuperExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitThisExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitUnaryExprFn: fn (*anyopaque, *const Expr) anyerror!void,
    visitVariableExprFn: fn (*anyopaque, *const Expr) anyerror!void,

    pub fn visitAssignExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitAssignExprFn(iface.impl, expr);
        return;
    }
    pub fn visitBinaryExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitBinaryExprFn(iface.impl, expr);
        return;
    }
    pub fn visitCallExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitCallExprFn(iface.impl, expr);
        return;
    }
    pub fn visitGetExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitGetExprFn(iface.impl, expr);
        return;
    }
    pub fn visitGroupingExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitGroupingExprFn(iface.impl, expr);
        return;
    }
    pub fn visitLiteralExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitLiteralExprFn(iface.impl, expr);
        return;
    }
    pub fn visitLogicalExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitLogicalExprFn(iface.impl, expr);
        return;
    }
    pub fn visitSetExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitSetExprFn(iface.impl, expr);
        return;
    }
    pub fn visitSuperExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitSuperExprFn(iface.impl, expr);
        return;
    }
    pub fn visitThisExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitThisExprFn(iface.impl, expr);
        return;
    }
    pub fn visitUnaryExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitUnaryExprFn(iface.impl, expr);
        return;
    }
    pub fn visitVariableExpr(iface: *VisitorInterface, expr: *const Expr) anyerror!void {
        try iface.visitVariableExprFn(iface.impl, expr);
        return;
    }
};

/// NOTES:
///  * We need introspection into the union in parser.Parser.assignment()
///     Given the state at the time we don't now what field is active in the
///     union so need to store an ExprType as part of Expr so we can handle the
///     type. Initially the goal was to avoid having to do this.
pub const ExprType = enum {
    assign,
    binary,
    call,
    get,
    grouping,
    literal,
    logical,
    set,
    super,
    this,
    unary,
    variable,
};

/// NOTES:
///  * We use the interface pattern here so we can build structures of Expr types
///     and cast their implementations back to their original types to operate on
///     them. Additionally the visitor pattern is also using the interface pattern.
///  * It is a compile error to use !void instead of anyerror!void. We should be
///     able to define proper errors once we clean up our usages of try.
pub const Expr = union (ExprType) {
    const Self = @This();

    assign: Assign,
    binary: Binary,
    call: Call,
    get: Get,
    grouping: Grouping,
    literal: Literal,
    logical: Logical,
    set: Set,
    super: Super,
    this: This,
    unary: Unary,
    variable: Variable,

    pub fn accept(self: *const Self, visitor: *VisitorInterface) anyerror!void {
        switch (self.*) {
            .assign => try Assign.accept(self, visitor),
            .binary => try Binary.accept(self, visitor),
            .call => try Call.accept(self, visitor),
            .get => try Get.accept(self, visitor),
            .grouping => try Grouping.accept(self, visitor),
            .literal => try Literal.accept(self, visitor),
            .logical => try Logical.accept(self, visitor),
            .set => try Set.accept(self, visitor),
            .super => try Super.accept(self, visitor),
            .this => try This.accept(self, visitor),
            .unary => try Unary.accept(self, visitor),
            .variable => try Variable.accept(self, visitor),
        }
    }

    pub fn create(allocator: std.mem.Allocator, any_expr: anytype) *Self {
        var ptr = allocator.create(Self) catch unreachable;

        switch (@TypeOf(any_expr)) {
            Assign => ptr.* = .{.assign = any_expr},
            Binary => ptr.* = .{.binary = any_expr},
            Call => ptr.* = .{.call = any_expr},
            Get => ptr.* = .{.get = any_expr},
            Grouping => ptr.* = .{.grouping = any_expr},
            Literal => ptr.* = .{.literal = any_expr},
            Logical => ptr.* = .{.logical = any_expr},
            Set => ptr.* = .{.set = any_expr},
            Super => ptr.* = .{.super = any_expr},
            This => ptr.* = .{.this = any_expr},
            Unary => ptr.* = .{.unary = any_expr},
            Variable => ptr.* = .{.variable = any_expr},
            else => @compileError("Not a valid Expr Type"),
        }

        return ptr;
    }
};

pub const Assign = struct {
    const Self = @This();
    name: *const Token,
    value: *const Expr,

    pub fn init(name: *const Token, value: *const Expr) Self {
        return .{
            .name = name,
            .value = value,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitAssignExpr(expr);
    }
};

pub const Binary = struct {
    const Self = @This();
    left: *const Expr,
    operator: *const Token,
    right: *const Expr,

    pub fn init(left: *const Expr, operator: *const Token, right: *const Expr) Self {
        return .{
            .left = left,
            .operator = operator,
            .right = right,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitBinaryExpr(expr);
    }
};

pub const Call = struct {
    const Self = @This();
    callee: *const Expr,
    paren: *const Token,
    arguments: std.ArrayList(*const Expr),

    pub fn init(callee: *const Expr, paren: *const Token, arguments: std.ArrayList(*const Expr)) Self {
        return .{
            .callee = callee,
            .paren = paren,
            .arguments = arguments,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitCallExpr(expr);
    }
};

pub const Get = struct {
    const Self = @This();
    object: *const Expr,
    name: *const Token,

    pub fn init(object: *const Expr, name: *const Token) Self {
        return .{
            .object = object,
            .name = name,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitGetExpr(expr);
    }
};

pub const Grouping = struct {
    const Self = @This();
    expression: *const Expr,

    pub fn init(expression: *const Expr) Self {
        return .{
            .expression = expression,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitGroupingExpr(expr);
    }
};

pub const Literal = struct {
    const Self = @This();
    value: Object,

    pub fn init(value: Object) Self {
        return .{
            .value = value,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitLiteralExpr(expr);
    }
};

pub const Logical = struct {
    const Self = @This();
    left: *const Expr,
    operator: *const Token,
    right: *const Expr,

    pub fn init(left: *const Expr, operator: *const Token, right: *const Expr) Self {
        return .{
            .left = left,
            .operator = operator,
            .right = right,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitLogicalExpr(expr);
    }
};

pub const Set = struct {
    const Self = @This();
    object: *const Expr,
    name: *const Token,
    value: *const Expr,

    pub fn init(object: *const Expr, name: *const Token, value: *const Expr) Self {
        return .{
            .object = object,
            .name = name,
            .value = value,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitSetExpr(expr);
    }
};

pub const Super = struct {
    const Self = @This();
    keyword: *const Token,
    method: *const Token,

    pub fn init(keyword: *const Token, method: *const Token) Self {
        return .{
            .keyword = keyword,
            .method = method,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitSuperExpr(expr);
    }
};

pub const This = struct {
    const Self = @This();
    keyword: *const Token,

    pub fn init(keyword: *const Token) Self {
        return .{
            .keyword = keyword,
        };
    }
    
    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitThisExpr(expr);
    }
};

pub const Unary = struct {
    const Self = @This();
    operator: *const Token,
    right: *const Expr,

    pub fn init(operator: *const Token, right: *const Expr) Self {
        return .{
            .operator = operator,
            .right = right,
        };
    }
    
    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitUnaryExpr(expr);
    }
};

pub const Variable = struct {
    const Self = @This();
    name: *const Token,

    pub fn init(name: *const Token) Self {
        return .{
            .name = name,
        };
    }

    fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try visitor.visitVariableExpr(expr);
    }
};

test "Expr.ptrs" {
    const a = std.testing.allocator;
    var x = Expr.create(a, Literal.init(Object.initNil()));
    defer a.destroy(x);
    try std.testing.expect(x.* == .literal);
    var y = Expr.create(a, Literal.init(Object.initNil()));
    defer a.destroy(y);
    var token = Token{
        .token_type = .MINUS,
        .lexeme = "-",
        .literal = Object.initNil(),
        .line = 1,
    };
    var z = Expr.create(a, Binary.init(x, &token, y));
    defer a.destroy(z);
    try std.testing.expect(z.* == .binary);
}
