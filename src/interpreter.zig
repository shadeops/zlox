const std = @import("std");
const Token = @import("token.zig").Token;
const LoxFunction = @import("function.zig").LoxFunction;
const LoxClass = @import("class.zig").LoxClass;
const LoxCallable = @import("callable.zig").LoxCallable;
const LoxInstance = @import("instance.zig").LoxInstance;
const Environment = @import("environment.zig").Environment;
const Object = @import("object.zig").Object;
const Expr = @import("expr.zig");
const Stmt = @import("stmt.zig");

const runtimeError = @import("main.zig").runtimeError;

fn castToSelf(comptime T: type, ptr: *anyopaque) *T {
    const alignment = @alignOf(T);
    const self = @ptrCast(*T, @alignCast(alignment, ptr));
    return self;
}

/// NOTES:
///  * In jlox defining the clock function is done directly in the Interpreter's
///     constructor. Here we are doing it outside due to needing to be a bit more
///     verbose in its construction.
const ClockCall = struct {
    const Self = @This();
    arity: u8 = 0,

    pub fn create(allocator: std.mem.Allocator) *Self {
        var ptr = allocator.create(Self) catch unreachable;
        ptr.* = Self{};
        return ptr;
    }

    pub fn toCallable(self: *Self) LoxCallable {
        return .{
            .impl = @ptrCast(*const anyopaque, self),
            .arity = self.arity,
            .callable_type = .FUNCTION,
            .callFn = call,
            .toStringFn = toString,
        };
    }

    fn call(
        ptr: *const anyopaque,
        interpreter: *Interpreter,
        arguments: std.ArrayList(Object),
    ) anyerror!Object {
        _ = ptr;
        _ = interpreter;
        _ = arguments;
        return Object.initNumber(@intToFloat(f64, std.time.milliTimestamp()) / 1000.0);
    }

    fn toString(ptr: *const anyopaque) []const u8 {
        _ = ptr;
        return "<native fn>";
    }
};

pub const Interpreter = struct {
    const Self = @This();
    allocator: std.mem.Allocator,
    environment: *Environment,
    globals: *Environment,
    locals: std.AutoHashMap(Expr.Expr, usize),

    // we capture the return value here instead of returning
    // a generic structure
    ret: ?Object = null,

    pub fn init(allocator: std.mem.Allocator) Self {
        var globals = allocator.create(Environment) catch unreachable;
        globals.* = Environment.init(allocator);

        var clockcall = ClockCall.create(allocator);
        var callable = allocator.create(LoxCallable) catch unreachable;
        callable.* = clockcall.toCallable();
        globals.define("clock", Object.initCallable(callable));

        return .{
            .allocator = allocator,
            .environment = globals,
            .globals = globals,
            .locals = std.AutoHashMap(Expr.Expr, usize).init(allocator),
        };
    }

    pub fn deinit(self: *Self) void {
        self.environment.deinit();
        self.locals.deinit();
        self.allocator.destroy(self.globals);
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

    // TODO: Similarly noted in main.zig, handling of errors is a bit out of sync
    // with jlox. Ideally we behave in a similar manner but for now is being
    // neglected while figuring out other details.
    pub fn interpret(self: *Self, statements: std.ArrayList(Stmt.Stmt)) void {
        for (statements.items) |statement| {
            self.execute(statement) catch |err| {
                runtimeError(@errorName(err));
            };
        }
    }

    fn visitAssignExpr(ptr: *anyopaque, expr: *const Expr.Assign) anyerror!void {
        const self = castToSelf(Self, ptr);

        var value = try self.evaluate(expr.value);

        var distance = self.locals.get(expr.toExpr());
        if (distance != null) {
            self.environment.assignAt(distance.?, expr.name, value);
        } else {
            try self.globals.assign(expr.name, value);
        }
        self.ret = value;
    }

    fn visitBinaryExpr(ptr: *anyopaque, expr: *const Expr.Binary) anyerror!void {
        const self = castToSelf(Self, ptr);

        var left = try self.evaluate(expr.left);
        var right = try self.evaluate(expr.right);

        switch (expr.operator.token_type) {
            .GREATER => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initBoolean(left.number > right.number);
            },
            .GREATER_EQUAL => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initBoolean(left.number >= right.number);
            },
            .LESS => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initBoolean(left.number < right.number);
            },
            .LESS_EQUAL => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initBoolean(left.number <= right.number);
            },
            .MINUS => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initNumber(left.number - right.number);
            },
            .PLUS => {
                if (left == .number and right == .number) {
                    self.ret = Object.initNumber(left.number + right.number);
                    return;
                }
                if (left == .string and right == .string) {
                    var strs = [_][]const u8{ left.string, right.string };
                    var new_str = std.mem.concat(self.allocator, u8, &strs) catch unreachable;
                    self.ret = Object.initString(new_str);
                    return;
                }
                std.log.err("Operand must be two numbers or two strings.", .{});
                return error.OperandError;
            },
            .SLASH => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initNumber(left.number / right.number);
            },
            .STAR => {
                try checkNumberOperands(expr.operator, left, right);
                self.ret = Object.initNumber(left.number * right.number);
            },
            .BANG_EQUAL => self.ret = Object.initBoolean(!isEqual(left, right)),
            .EQUAL_EQUAL => self.ret = Object.initBoolean(isEqual(left, right)),
            else => unreachable,
        }
        return;
    }

    fn visitCallExpr(ptr: *anyopaque, expr: *const Expr.Call) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;

        var callee = try self.evaluate(expr.callee);

        var arguments = std.ArrayList(Object).init(self.allocator);
        for (expr.arguments.items) |argument| {
            try arguments.append(try self.evaluate(argument));
        }

        if (callee != .callable) {
            std.log.err("Can only call functions and classes.", .{});
            return error.CallableError;
        }

        var function = callee.callable;

        if (arguments.items.len != function.arity) {
            std.log.err(
                "Expected {} arguments but got {}.",
                .{ function.arity, arguments.items.len },
            );
            return error.CallableError;
        }

        self.ret = try function.call(self, arguments);
    }

    fn visitGetExpr(ptr: *anyopaque, expr: *const Expr.Get) anyerror!void {
        const self = castToSelf(Self, ptr);
        var object = try self.evaluate(expr.object);
        if (object == .instance) {
            self.ret = try object.instance.get(expr.name);
            return;
        }
        std.log.err("Only instances have properties, {}", .{expr.name});
        return error.RuntimeError;
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
        const self = castToSelf(Self, ptr);
        var left = try self.evaluate(expr.left);

        if (expr.operator.token_type == .OR) {
            if (isTruthy(left)) {
                self.ret = left;
                return;
            }
        } else {
            if (!isTruthy(left)) {
                self.ret = left;
                return;
            }
        }

        self.ret = try self.evaluate(expr.right);
    }

    fn visitSetExpr(ptr: *anyopaque, expr: *const Expr.Set) anyerror!void {
        const self = castToSelf(Self, ptr);

        var object = try self.evaluate(expr.object);
        if (object != .instance) {
            std.log.err("Only instances have fields, {s}", .{expr.name});
            return error.RuntimeError;
        }
        var value = try self.evaluate(expr.value);
        try object.instance.set(expr.name, value);
        self.ret = value;
    }

    fn visitSuperExpr(ptr: *anyopaque, expr: *const Expr.Super) anyerror!void {
        const self = castToSelf(Self, ptr);
        var distance = self.locals.get(expr.toExpr()) orelse {
            std.log.err("Could not evaluate distance\n", .{});
            unreachable;
        };
        var superclass_obj = try self.environment.getAt(distance, "super");

        const class_alignment = @alignOf(LoxClass);
        var superclass = @ptrCast(*const LoxClass, @alignCast(
            class_alignment,
            superclass_obj.callable.impl,
        ));

        var object = try self.environment.getAt(distance - 1, "this");
        const instance_alignment = @alignOf(LoxInstance);
        var instance = @ptrCast(*LoxInstance, @alignCast(
            instance_alignment,
            object.instance,
        ));

        var method = superclass.findMethod(expr.method.lexeme);
        if (method == null) {
            std.log.err("Undefined property '{s}'.", .{expr.method.lexeme});
            return error.RunTimeError;
        }
        self.ret = Object.initCallable(&method.?.bind(instance).toCallable());
    }

    fn visitThisExpr(ptr: *anyopaque, expr: *const Expr.This) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = try self.lookUpVariable(expr.keyword, expr.toExpr());
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
                self.ret = Object.initNumber(-right.number);
            },
            else => {
                self.ret = null;
            },
        }
    }

    fn visitVariableExpr(ptr: *anyopaque, expr: *const Expr.Variable) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = try self.lookUpVariable(expr.name, expr.toExpr());
    }

    fn lookUpVariable(self: *Self, name: Token, expr: Expr.Expr) !Object {
        var distance = self.locals.get(expr);
        if (distance != null) {
            return try self.environment.getAt(distance.?, name.lexeme);
        }
        return self.globals.get(name);
    }

    fn checkNumberOperand(operator: Token, operand: Object) !void {
        _ = operator;
        if (operand == .number) return;
        std.log.err("Operand must be a number.", .{});
        return error.OperandError;
    }

    fn checkNumberOperands(operator: Token, left: Object, right: Object) !void {
        _ = operator;
        if (left == .number and right == .number) return;
        std.log.err("Operands must be a number.", .{});
        return error.OperandError;
    }

    fn evaluate(self: *Self, expr: Expr.Expr) !Object {
        var iface = self.exprInterface();
        // Whichever Expr accepts the Interpreter Interface which will
        // update the Interpreter's ret value
        try expr.accept(&iface);
        // while we are setting self.ret to null in various places
        // if we hit one something has gone wrong as it should be unreachable.
        return self.ret orelse unreachable;
    }

    fn execute(self: *Self, stmt: Stmt.Stmt) !void {
        var iface = self.stmtInterface();
        try stmt.accept(&iface);
    }

    pub fn resolve(self: *Self, expr: Expr.Expr, depth: usize) !void {
        try self.locals.put(expr, depth);
    }

    pub fn executeBlock(
        self: *Self,
        statements: std.ArrayList(Stmt.Stmt),
        environment: *Environment,
    ) !void {
        var previous = self.environment;
        defer self.environment = previous;

        self.environment = environment;
        for (statements.items) |statement| {
            try self.execute(statement);
        }
    }

    fn visitBlockStmt(ptr: *anyopaque, stmt: *const Stmt.Block) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;

        var environment = try self.allocator.create(Environment);
        defer self.allocator.destroy(environment);
        environment.* = Environment.init(self.allocator);
        environment.enclosing = self.environment;
        try self.executeBlock(stmt.statements, environment);
    }

    fn visitClassStmt(ptr: *anyopaque, stmt: *const Stmt.Class) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;

        var superclass: ?*const LoxClass = null;
        if (stmt.superclass != null) {
            var superclass_obj = try self.evaluate(stmt.superclass.?.toExpr());
            if (superclass_obj != .callable or superclass_obj.callable.callable_type != .CLASS) {
                std.log.err("Superclass must be a class, {s}", .{stmt.superclass.?.name});
                return error.RuntimeError;
            } else {
                const alignment = @alignOf(LoxClass);
                superclass = @ptrCast(*const LoxClass, @alignCast(
                    alignment,
                    superclass_obj.callable.impl,
                ));
            }
        }

        self.environment.define(stmt.name.lexeme, Object.initNil());

        if (stmt.superclass != null) {
            var new_environment = try self.allocator.create(Environment);
            new_environment.* = Environment.init(self.allocator);
            new_environment.enclosing = self.environment;
            self.environment = new_environment;

            // Scope?
            self.environment.define("super", Object.initCallable(&superclass.?.toCallable()));
        }

        var methods = std.StringHashMap(*LoxFunction).init(self.allocator);
        for (stmt.methods.items) |method| {
            var function = LoxFunction.create(
                self.allocator,
                method.*,
                self.environment,
                std.mem.eql(u8, method.name.lexeme, "init"),
            );
            try methods.put(method.name.lexeme, function);
        }

        var callable_ptr = self.allocator.create(LoxCallable) catch unreachable;
        callable_ptr.* = LoxClass.create(
            self.allocator,
            stmt.name.lexeme,
            superclass,
            methods,
        ).toCallable();
        var callable = Object.initCallable(callable_ptr);

        if (superclass != null) {
            self.environment = self.environment.enclosing.?;
        }

        try self.environment.assign(stmt.name, callable);
    }

    fn visitExpressionStmt(ptr: *anyopaque, stmt: *const Stmt.Expression) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;
        _ = try self.evaluate(stmt.expression);
    }

    fn visitFunctionStmt(ptr: *anyopaque, stmt: *const Stmt.Function) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;
        var callable_ptr = self.allocator.create(LoxCallable) catch unreachable;
        callable_ptr.* = LoxFunction.create(
            self.allocator,
            stmt.*,
            self.environment,
            false,
        ).toCallable();
        var function = Object.initCallable(callable_ptr);
        self.environment.define(stmt.name.lexeme, function);
    }

    fn visitIfStmt(ptr: *anyopaque, stmt: *const Stmt.If) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;
        if (isTruthy(try self.evaluate(stmt.condition))) {
            try self.execute(stmt.then_branch);
        } else if (stmt.else_branch != null) {
            try self.execute(stmt.else_branch.?);
        }
    }

    fn visitPrintStmt(ptr: *anyopaque, stmt: *const Stmt.Print) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;
        var value = try self.evaluate(stmt.expression);
        self.stringify(value);
    }

    fn visitReturnStmt(ptr: *anyopaque, stmt: *const Stmt.Return) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;

        var value = Object.initNil();
        if (stmt.value != null)
            value = try self.evaluate(stmt.value.?);

        self.ret = value;
        // Horrible control flow hack that jlox uses. Use an exception to walk
        // back up the stack. Here we'll use a specific error, and rely on storing
        // the value in self.ret
        return error.ReturnValue;
    }

    fn visitVarStmt(ptr: *anyopaque, stmt: *const Stmt.Var) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;

        var value = Object.initNil();
        if (stmt.initializer != null) {
            value = try self.evaluate(stmt.initializer.?);
        }

        self.environment.define(stmt.name.lexeme, value);
    }

    fn visitWhileStmt(ptr: *anyopaque, stmt: *const Stmt.While) anyerror!void {
        const self = castToSelf(Self, ptr);
        self.ret = null;

        while (isTruthy(try self.evaluate(stmt.condition))) {
            try self.execute(stmt.body);
        }
    }

    fn isTruthy(object: Object) bool {
        switch (object) {
            .nil => return false,
            .boolean => |value| return value,
            else => return true,
        }
    }

    fn isEqual(a: Object, b: Object) bool {
        if (a == .nil and b == .nil) return true;
        if (a == .nil or b == .nil) return false;
        if (!a.isSameType(b)) return false;
        switch (a) {
            .number => |value| return value == b.number,
            .boolean => |value| return value == b.boolean,
            .string => |value| return std.mem.eql(u8, value, b.string),
            else => return false,
        }
        return false;
    }

    fn stringify(self: *Self, object: Object) void {
        const stdout = std.io.getStdOut();
        const writer = stdout.writer();
        writer.print("{s}\n", .{object.toString(self.allocator)}) catch unreachable;
    }
};
