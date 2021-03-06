const std = @import("std");
const Token = @import("token.zig").Token;
const Object = @import("object.zig").Object;

pub const VisitorInterface = struct {
    impl: *anyopaque,

    visitAssignExprFn: fn (*anyopaque, *const Assign) anyerror!void,
    visitBinaryExprFn: fn (*anyopaque, *const Binary) anyerror!void,
    visitCallExprFn: fn (*anyopaque, *const Call) anyerror!void,
    visitGetExprFn: fn (*anyopaque, *const Get) anyerror!void,
    visitGroupingExprFn: fn (*anyopaque, *const Grouping) anyerror!void,
    visitLiteralExprFn: fn (*anyopaque, *const Literal) anyerror!void,
    visitLogicalExprFn: fn (*anyopaque, *const Logical) anyerror!void,
    visitSetExprFn: fn (*anyopaque, *const Set) anyerror!void,
    visitSuperExprFn: fn (*anyopaque, *const Super) anyerror!void,
    visitThisExprFn: fn (*anyopaque, *const This) anyerror!void,
    visitUnaryExprFn: fn (*anyopaque, *const Unary) anyerror!void,
    visitVariableExprFn: fn (*anyopaque, *const Variable) anyerror!void,

    pub fn visitAssignExpr(iface: *VisitorInterface, expr: *const Assign) anyerror!void {
        try iface.visitAssignExprFn(iface.impl, expr);
        return;
    }
    pub fn visitBinaryExpr(iface: *VisitorInterface, expr: *const Binary) anyerror!void {
        try iface.visitBinaryExprFn(iface.impl, expr);
        return;
    }
    pub fn visitCallExpr(iface: *VisitorInterface, expr: *const Call) anyerror!void {
        try iface.visitCallExprFn(iface.impl, expr);
        return;
    }
    pub fn visitGetExpr(iface: *VisitorInterface, expr: *const Get) anyerror!void {
        try iface.visitGetExprFn(iface.impl, expr);
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
    pub fn visitLogicalExpr(iface: *VisitorInterface, expr: *const Logical) anyerror!void {
        try iface.visitLogicalExprFn(iface.impl, expr);
        return;
    }
    pub fn visitSetExpr(iface: *VisitorInterface, expr: *const Set) anyerror!void {
        try iface.visitSetExprFn(iface.impl, expr);
        return;
    }
    pub fn visitSuperExpr(iface: *VisitorInterface, expr: *const Super) anyerror!void {
        try iface.visitSuperExprFn(iface.impl, expr);
        return;
    }
    pub fn visitThisExpr(iface: *VisitorInterface, expr: *const This) anyerror!void {
        try iface.visitThisExprFn(iface.impl, expr);
        return;
    }
    pub fn visitUnaryExpr(iface: *VisitorInterface, expr: *const Unary) anyerror!void {
        try iface.visitUnaryExprFn(iface.impl, expr);
        return;
    }
    pub fn visitVariableExpr(iface: *VisitorInterface, expr: *const Variable) anyerror!void {
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
    ASSIGN,
    BINARY,
    CALL,
    GET,
    GROUPING,
    LITERAL,
    LOGICAL,
    SET,
    SUPER,
    THIS,
    UNARY,
    VARIABLE,
};

/// NOTES:
///  * We use the interface pattern here so we can build structures of Expr types
///     and cast their implementations back to their original types to operate on
///     them. Additionally the visitor pattern is also using the interface pattern.
///  * It is a compile error to use !void instead of anyerror!void. We should be
///     able to define proper errors once we clean up our usages of try.
pub const Expr = struct {
    impl: *const anyopaque,
    expr_type: ExprType,

    acceptFn: fn (*const anyopaque, *VisitorInterface) anyerror!void,

    pub fn accept(expr: *const Expr, visitor: *VisitorInterface) anyerror!void {
        try expr.acceptFn(expr.impl, visitor);
    }
};

/// Helper function to cast an opaque interface pointer to the proper type
fn castToConstSelf(comptime T: type, ptr: *const anyopaque) *const T {
    const alignment = @alignOf(T);
    const self = @ptrCast(*const T, @alignCast(alignment, ptr));
    return self;
}

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

    /// Creator owns the memory and is responsible for destroying it
    pub fn create(allocator: std.mem.Allocator, name: Token, value: Expr) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(name, value);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .ASSIGN,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(
        allocator: std.mem.Allocator,
        left: Expr,
        operator: Token,
        right: Expr,
    ) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(left, operator, right);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .BINARY,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(
        allocator: std.mem.Allocator,
        callee: Expr,
        paren: Token,
        arguments: std.ArrayList(Expr),
    ) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(callee, paren, arguments);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .CALL,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(allocator: std.mem.Allocator, object: Expr, name: Token) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(object, name);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .GET,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(allocator: std.mem.Allocator, expression: Expr) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(expression);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .GROUPING,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitGroupingExpr(self);
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

    pub fn create(allocator: std.mem.Allocator, value: Object) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(value);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .LITERAL,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(allocator: std.mem.Allocator, left: Expr, operator: Token, right: Expr) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(left, operator, right);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .LOGICAL,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(allocator: std.mem.Allocator, object: Expr, name: Token, value: Expr) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(object, name, value);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .SET,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(allocator: std.mem.Allocator, keyword: Token, method: Token) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(keyword, method);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .SUPER,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(allocator: std.mem.Allocator, keyword: Token) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(keyword);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .THIS,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(allocator: std.mem.Allocator, operator: Token, right: Expr) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(operator, right);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .UNARY,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
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

    pub fn create(allocator: std.mem.Allocator, name: Token) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self.init(name);
        return ptr;
    }

    pub fn toExpr(self: *const Self) Expr {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .acceptFn = accept,
            .expr_type = .VARIABLE,
        };
    }

    fn accept(ptr: *const anyopaque, visitor: *VisitorInterface) anyerror!void {
        const self = castToConstSelf(Self, ptr);
        try visitor.visitVariableExpr(self);
    }
};

test "Expr.ptrs" {
    const a = std.testing.allocator;
    var x = Literal.create(a, Object.initNil());
    var interface = x.toExpr();
    try std.testing.expect(interface.expr_type == .LITERAL);
    std.debug.print("\n", .{});
    try std.testing.expect(@ptrToInt(x) == @ptrToInt(interface.impl));
    std.debug.print("x: {*}\n", .{x});
    std.debug.print("&x: {}\n", .{&x});
    std.debug.print("impl {*}\n", .{(interface.impl)});
    std.debug.print("&impl {}\n", .{&(interface.impl)});
    a.destroy(x);
    x = Literal.create(a, Object.initNil());
    //try std.testing.expect(@ptrToInt(x) == @ptrToInt(interface.impl));
    std.debug.print("x: {*}\n", .{x});
    std.debug.print("&x: {}\n", .{&x});
    std.debug.print("impl {*}\n", .{(interface.impl)});
    std.debug.print("&impl {}\n", .{&(interface.impl)});
    a.destroy(x);
}
