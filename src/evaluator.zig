const std = @import("std");
const ast = @import("ast.zig");
const tok = @import("token.zig");

const obj = @import("obj.zig");

// Builtin functions
fn builtinPuts(allocator: std.mem.Allocator, args: []const obj.Object) anyerror!obj.Object {
    var stdout_buffer: [4096]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const writer = &stdout_writer.interface;
    for (args) |arg| {
        try arg.inspect(writer);
        try writer.writeByte('\n');
        try writer.flush();
    }
    _ = allocator;
    return obj.Object{ .@"null" = obj.NullObject{ .value = {} } };
}

pub const EvalError = error{
    OutOfMemory,
    VariableNotFound,
} || std.mem.Allocator.Error || std.Io.Writer.Error;

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
        var eval = Evaluator{
            .heap = heap,
            .env = Environment.init(heap),
        };
        eval.initBuiltins() catch {};
        return eval;
    }

    fn initBuiltins(self: *Evaluator) EvalError!void {
        const puts_obj = obj.Object{ .builtin = obj.BuiltinObject{ .@"fn" = &builtinPuts } };
        try self.env.set("puts", puts_obj);
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
            // Stop evaluation if we hit an error object
            if (result) |result_obj| {
                if (result_obj == .@"error") {
                    return result;
                }
            }
        }
        return result;
    }

    fn evaluateStatement(self: *Evaluator, stmt: ast.Statement) EvalError!?obj.Object {
        return switch (stmt) {
            .let_statement => |let_stmt| {
                const result = try self.evaluateLetStatement(let_stmt);
                return result;
            },
            .expression_statement => |exp_stmt| {
                return try self.evaluateExpression(exp_stmt.expression);
            },
        };
    }

    fn evaluateLetStatement(self: *Evaluator, stmt: ast.LetStatement) EvalError!?obj.Object {
        const value = try self.evaluateExpression(stmt.value);
        // Check for error objects - return them so they propagate
        if (value == .@"error") {
            return value;
        }
        const name = stmt.name.value;
        try self.env.set(name, value);
        return null;
    }

    fn evaluateExpression(self: *Evaluator, expr: ast.Expression) EvalError!obj.Object {
        return switch (expr) {
            .identifier => |ident| {
                return self.evaluateIdentifier(ident);
            },
            .integer_literal => |int_lit| {
                return self.evaluateIntegerLiteral(int_lit);
            },
            .string_literal => |str_lit| {
                return self.evaluateStringLiteral(str_lit);
            },
            .prefix_expression => |prefix_expr| {
                return try self.evaluatePrefixExpression(prefix_expr);
            },
            .infix_expression => |infix_expr| {
                return try self.evaluateInfixExpression(infix_expr);
            },
            .call_expression => |call_expr| {
                return try self.evaluateCallExpression(call_expr);
            },
        };
    }

    fn evaluateIdentifier(self: *Evaluator, ident: ast.Identifier) EvalError!obj.Object {
        const name = ident.value;
        if (self.env.get(name)) |value| {
            return value;
        } else {
            // Variable not found - return error object
            const msg = try std.fmt.allocPrint(self.heap, "identifier not found: {s}", .{name});
            return obj.Object{ .@"error" = obj.ErrorObject{ .message = msg } };
        }
    }

    fn evaluateIntegerLiteral(_: *Evaluator, int_lit: ast.IntegerLiteral) EvalError!obj.Object {
        return obj.Object{ .integer = obj.IntegerObject{ .value = int_lit.value } };
    }

    fn evaluateStringLiteral(_: *Evaluator, str_lit: ast.StringLiteral) EvalError!obj.Object {
        return obj.Object{ .string = obj.StringObject{ .value = str_lit.value } };
    }

    fn evaluatePrefixExpression(self: *Evaluator, expr: ast.PrefixExpression) EvalError!obj.Object {
        const right = try self.evaluateExpression(expr.right.*);
        // Check for error objects
        if (right == .@"error") {
            return right;
        }

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
        // Check for error objects
        if (left_obj == .@"error") {
            return left_obj;
        }
        
        const right_obj = try self.evaluateExpression(expr.right.*);
        // Check for error objects
        if (right_obj == .@"error") {
            return right_obj;
        }

        // Handle + operator: integer addition or string concatenation
        if (std.mem.eql(u8, expr.operator, "+")) {
            // If both are integers, do integer addition
            if (left_obj == .integer and right_obj == .integer) {
                const left_val = left_obj.integer.value;
                const right_val = right_obj.integer.value;
                return obj.Object{ .integer = obj.IntegerObject{ .value = left_val + right_val } };
            }
            
            // If both are strings, concatenate directly
            if (left_obj == .string and right_obj == .string) {
                const left_str = left_obj.string.value;
                const right_str = right_obj.string.value;
                const total_len = left_str.len + right_str.len;
                const concat = try self.heap.alloc(u8, total_len);
                @memcpy(concat[0..left_str.len], left_str);
                @memcpy(concat[left_str.len..], right_str);
                return obj.Object{ .string = obj.StringObject{ .value = concat } };
            }
            
            // Otherwise, convert to strings and concatenate
            // Use fixed buffers for integer conversion to avoid intermediate allocations
            var left_buffer: [64]u8 = undefined;
            var right_buffer: [64]u8 = undefined;
            
            const left_str = blk: {
                switch (left_obj) {
                    .string => |str_obj| break :blk str_obj.value,
                    .integer => |int_obj| {
                        const str = std.fmt.bufPrint(&left_buffer, "{d}", .{int_obj.value}) catch {
                            const msg = try self.heap.dupe(u8, "integer too large to convert");
                            return obj.Object{ .@"error" = obj.ErrorObject{ .message = msg } };
                        };
                        break :blk str;
                    },
                    else => return obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
                }
            };
            
            const right_str = blk: {
                switch (right_obj) {
                    .string => |str_obj| break :blk str_obj.value,
                    .integer => |int_obj| {
                        const str = std.fmt.bufPrint(&right_buffer, "{d}", .{int_obj.value}) catch {
                            const msg = try self.heap.dupe(u8, "integer too large to convert");
                            return obj.Object{ .@"error" = obj.ErrorObject{ .message = msg } };
                        };
                        break :blk str;
                    },
                    else => return obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
                }
            };

            // Allocate once for the concatenated string
            const total_len = left_str.len + right_str.len;
            const concat = try self.heap.alloc(u8, total_len);
            @memcpy(concat[0..left_str.len], left_str);
            @memcpy(concat[left_str.len..], right_str);
            
            return obj.Object{ .string = obj.StringObject{ .value = concat } };
        }

        // Both operands must be integers for other arithmetic operations
        const left = switch (left_obj) {
            .integer => |int_obj| int_obj.value,
            else => return obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
        };
        const right = switch (right_obj) {
            .integer => |int_obj| int_obj.value,
            else => return obj.Object{ .@"null" = obj.NullObject{ .value = {} } },
        };

        return switch (expr.operator[0]) {
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
        // Check for error objects
        if (value == .@"error") {
            return value;
        }
        const name = left_ident.value;
        try self.env.set(name, value);
        return value;
    }

    fn evaluateCallExpression(self: *Evaluator, expr: ast.CallExpression) EvalError!obj.Object {
        const function = try self.evaluateExpression(expr.function.*);
        
        // If function evaluation returned an error object, return it
        if (function == .@"error") {
            return function;
        }
        
        var args = std.ArrayList(obj.Object){};
        defer args.deinit(self.heap);
        
        for (expr.arguments.items) |arg| {
            const evaluated = try self.evaluateExpression(arg);
            // Check for error objects in arguments
            if (evaluated == .@"error") {
                return evaluated;
            }
            try args.append(self.heap, evaluated);
        }

        return switch (function) {
            .builtin => |builtin| {
                return builtin.@"fn"(self.heap, args.items) catch {
                    // Builtin functions should return error objects, not propagate errors
                    // Convert any error to an error object
                    const msg = try self.heap.dupe(u8, "builtin function error");
                    return obj.Object{ .@"error" = obj.ErrorObject{ .message = msg } };
                };
            },
            else => {
                const msg = try self.heap.dupe(u8, "not a function");
                return obj.Object{ .@"error" = obj.ErrorObject{ .message = msg } };
            },
        };
    }
};