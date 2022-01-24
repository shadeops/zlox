const std = @import("std");
const Token = @import("token.zig").Token;
const Object = @import("object.zig").Object;
const Expr = @import("expr.zig");

fn castToSelf(comptime T: type, ptr: *anyopaque) *T {
    const alignment = @alignOf(T);
    const self = @ptrCast(*T, @alignCast(alignment, ptr));
    return self;
}

pub const AstPrinter = struct {
    const Self = @This();
    strings: std.ArrayList([]const u8),
    string_buffer: std.ArrayList([]const u8),
    allocator: std.mem.Allocator,

    pub fn init(allocator: std.mem.Allocator) AstPrinter {
        return .{
            .strings = std.ArrayList([]const u8).init(allocator),
            .string_buffer = std.ArrayList([]const u8).init(allocator),
            .allocator = allocator,
        };
    }

    pub fn deinit(self: *AstPrinter) void {
        self.strings.deinit();
        for (self.string_buffer.items) |item| {
            self.allocator.free(item);
        }
        self.string_buffer.deinit();
    }

    pub fn interface(self: *Self) Expr.VisitorInterface {
        return .{
            .impl = @ptrCast(*anyopaque, self),
            .visitExprFn = print,
            .visitAssignExprFn = visitAssignExpr,
            .visitBinaryExprFn = visitBinaryExpr,
            .visitCallExprFn = visitCallExpr,
            .visitGetExprFn = visitGetExpr,
            .visitGroupingExprFn = visitGroupingExpr,
            .visitLiteralExprFn = visitLiteralExpr,
            .visitSetExprFn = visitSetExpr,
            .visitSuperExprFn = visitSuperExpr,
            .visitThisExprFn = visitThisExpr,
            .visitUnaryExprFn = visitUnaryExpr,
            .visitVariableExprFn = visitVariableExpr,
        };
    }

    pub fn print(ptr: *anyopaque, expr: *const Expr.Expr) anyerror!void {
        const self = castToSelf(Self, ptr);
        var iface = self.interface();
        try expr.accept(&iface);
        for (self.strings.items) |item| {
            std.debug.print("{s}", .{item});
        }
        std.debug.print("\n", .{});
    }

    fn visitAssignExpr(ptr: *anyopaque, expr: *const Expr.Assign) !void {
        _ = ptr;
        _ = expr;
    }

    fn visitBinaryExpr(ptr: *anyopaque, expr: *const Expr.Binary) anyerror!void {
        const self = castToSelf(Self, ptr);
        var iface = self.interface();
        try self.strings.append("(");
        try self.strings.append(expr.operator.lexeme);
        try self.strings.append(" ");
        try expr.left.accept(&iface);
        try self.strings.append(" ");
        try expr.right.accept(&iface);
        try self.strings.append(")");
    }

    fn visitCallExpr(ptr: *anyopaque, expr: *const Expr.Call) !void {
        _ = ptr;
        _ = expr;
    }
    fn visitGetExpr(ptr: *anyopaque, expr: *const Expr.Get) !void {
        _ = ptr;
        _ = expr;
    }
    fn visitGroupingExpr(ptr: *anyopaque, expr: *const Expr.Grouping) !void {
        const self = castToSelf(Self, ptr);
        var iface = self.interface();
        try self.strings.append("(group ");
        try expr.expression.accept(&iface);
        try self.strings.append(")");
    }
    fn visitLiteralExpr(ptr: *anyopaque, expr: *const Expr.Literal) !void {
        const self = castToSelf(Self, ptr);
        if (expr.value) |v| {
            var string = try v.toString(self.allocator);
            try self.strings.append(string);
            try self.string_buffer.append(string);
        } else {
            try self.strings.append("nil");
        }
    }
    fn visitSetExpr(ptr: *anyopaque, expr: *const Expr.Set) !void {
        _ = ptr;
        _ = expr;
    }
    fn visitSuperExpr(ptr: *anyopaque, expr: *const Expr.Super) !void {
        _ = ptr;
        _ = expr;
    }
    fn visitThisExpr(ptr: *anyopaque, expr: *const Expr.This) !void {
        _ = ptr;
        _ = expr;
    }
    fn visitUnaryExpr(ptr: *anyopaque, expr: *const Expr.Unary) !void {
        const self = castToSelf(Self, ptr);
        var iface = self.interface();
        try self.strings.append("(");
        try self.strings.append(expr.operator.lexeme);
        try self.strings.append(" ");
        try expr.right.accept(&iface);
        try self.strings.append(")");
    }
    fn visitVariableExpr(ptr: *anyopaque, expr: *const Expr.Variable) !void {
        _ = ptr;
        _ = expr;
    }
};

pub fn main() anyerror!void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();

    var expression = Expr.Binary.init(
        Expr.Unary.init(
            Token{
                .token_type = .MINUS,
                .lexeme = "-",
                .literal = null,
                .line = 1,
            },
            Expr.Literal.init(Object.initNumber(123)).toExpr(),
        ).toExpr(),
        Token{
            .token_type = .STAR,
            .lexeme = "*",
            .literal = null,
            .line = 1,
        },
        Expr.Grouping.init(
            Expr.Literal.init(Object.initNumber(45.67)).toExpr(),
        ).toExpr(),
    ).toExpr();
    var printer = AstPrinter.init(allocator);
    var printer_interface = printer.interface();
    try printer_interface.visitExpr(&expression);
    return;
}
