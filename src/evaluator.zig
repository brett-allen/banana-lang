const std = @import("std");
const ast = @import("ast.zig");
const tok = @import("token.zig");

const obj = @import("obj.zig");

pub const EvalError = error{
    OutOfMemory,
    VariableNotFound,
};

// Environment to store variable bindings
pub const Environment = struct {
    heap: std.mem.Allocator,
    store: std.StringHashMap(obj.Object),

    pub fn init(heap: std.mem.Allocator) Environment {
        return Environment{
            .heap = heap,
            .store = std.StringHashMap(obj.Object).init(heap),
        };
    }

    pub fn deinit(self: *Environment) void {
        self.store.deinit();
    }

    pub fn get(self: *const Environment, name: []const u8) ?obj.Object {
        return self.store.get(name);
    }

    pub fn set(self: *Environment, name: []const u8, value: obj.Object) !void {
        // Check if key already exists
        if (self.store.getEntry(name)) |entry| {
            // Key exists - just update the value
            entry.value_ptr.* = value;
        } else {
            // New key - allocate a copy and insert
            const name_copy = try self.heap.dupe(u8, name);
            try self.store.put(name_copy, value);
        }
    }
};

pub const Evaluator = struct {
    heap: std.mem.Allocator,
    env: Environment,

    pub fn init(heap: std.mem.Allocator) Evaluator {
        return Evaluator{
            .heap = heap,
            .env = Environment.init(heap),
        };
    }

    pub fn deinit(self: *Evaluator) void {
        self.env.deinit();
    }

    pub fn evaluate(self: *Evaluator, node: ast.Node) EvalError!?obj.Object {
        return switch (node) {
            .program => |program| try self.evaluateProgram(program),
            .statement => |stmt| try self.evaluateStatement(stmt),
            .expression => |expr| try self.evaluateExpression(expr),
        };
    }

    fn evaluateProgram(self: *Evaluator, program: ast.Program) EvalError!?obj.Object {
        var result: ?obj.Object = null;
        for (program.statements.items) |stmt| {
            result = try self.evaluateStatement(stmt);
        }
        return result;
    }

    fn evaluateStatement(self: *Evaluator, stmt: ast.Statement) EvalError!?obj.Object {
        return switch (stmt) {
            .let_statement => |let_stmt| {
                try self.evaluateLetStatement(let_stmt);
                return null;
            },
            .expression_statement => |exp_stmt| {
                return try self.evaluateExpression(exp_stmt.expression);
            },
        };
    }

    fn evaluateLetStatement(self: *Evaluator, stmt: ast.LetStatement) EvalError!void {
        const value = try self.evaluateExpression(stmt.value);
        const name = stmt.name.value;
        try self.env.set(name, value);
    }

    fn evaluateExpression(self: *Evaluator, expr: ast.Expression) EvalError!obj.Object {
        return switch (expr) {
            .identifier => |ident| {
                return self.evaluateIdentifier(ident);
            },
            .integer_literal => |int_lit| {
                return self.evaluateIntegerLiteral(int_lit);
            },
            .prefix_expression => |prefix_expr| {
                return try self.evaluatePrefixExpression(prefix_expr);
            },
            .infix_expression => |infix_expr| {
                return try self.evaluateInfixExpression(infix_expr);
            },
        };
    }

    fn evaluateIdentifier(self: *Evaluator, ident: ast.Identifier) EvalError!obj.Object {
        const name = ident.value;
        if (self.env.get(name)) |value| {
            return value;
        } else {
            // Variable not found - return null for now
            // In a full implementation, this should be an error
            return EvalError.VariableNotFound;
        }
    }

    fn evaluateIntegerLiteral(_: *Evaluator, int_lit: ast.IntegerLiteral) EvalError!obj.Object {
        return obj.Object{ .integer = obj.IntegerObject{ .value = int_lit.value } };
    }

    fn evaluatePrefixExpression(self: *Evaluator, expr: ast.PrefixExpression) EvalError!obj.Object {
        const right = try self.evaluateExpression(expr.right.*);

        return switch (expr.operator[0]) {
            '!' => {
                return switch (right) {
                    .integer => |int_obj| obj.Object{ .integer = obj.IntegerObject{ .value = if (int_obj.value == 0) 1 else 0 } },
                    else => obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
                };
            },
            '-' => {
                return switch (right) {
                    .integer => |int_obj| obj.Object{ .integer = obj.IntegerObject{ .value = -int_obj.value } },
                    else => obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
                };
            },
            else => obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
        };
    }

    fn evaluateInfixExpression(self: *Evaluator, expr: ast.InfixExpression) EvalError!obj.Object {
        // Handle assignment operator
        if (std.mem.eql(u8, expr.operator, "=")) {
            return try self.evaluateAssignment(expr);
        }

        const left_obj = try self.evaluateExpression(expr.left.*);
        const right_obj = try self.evaluateExpression(expr.right.*);

        // Both operands must be integers for arithmetic operations
        const left = switch (left_obj) {
            .integer => |int_obj| int_obj.value,
            else => return obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
        };
        const right = switch (right_obj) {
            .integer => |int_obj| int_obj.value,
            else => return obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
        };

        return switch (expr.operator[0]) {
            '+' => obj.Object{ .integer = obj.IntegerObject{ .value = left + right } },
            '-' => obj.Object{ .integer = obj.IntegerObject{ .value = left - right } },
            '*' => obj.Object{ .integer = obj.IntegerObject{ .value = left * right } },
            '/' => obj.Object{ .integer = obj.IntegerObject{ .value = @divTrunc(left, right) } },
            '<' => obj.Object{ .integer = obj.IntegerObject{ .value = if (left < right) 1 else 0 } },
            '>' => obj.Object{ .integer = obj.IntegerObject{ .value = if (left > right) 1 else 0 } },
            else => obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
        };
    }

    fn evaluateAssignment(self: *Evaluator, expr: ast.InfixExpression) EvalError!obj.Object {
        // Left side must be an identifier
        const left_ident = switch (expr.left.*) {
            .identifier => |ident| ident,
            else => return obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
        };

        const value = try self.evaluateExpression(expr.right.*);
        const name = left_ident.value;
        try self.env.set(name, value);
        return value;
    }
};