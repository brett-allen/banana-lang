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
    return obj.Object{ .null = obj.NullObject{ .value = {} } };
}

pub const EvalError = error{
    OutOfMemory,
    VariableNotFound,
} || std.mem.Allocator.Error || std.Io.Writer.Error;

// Environment to store variable bindings. Supports closure chain via outer.
pub const Environment = struct {
    heap: std.mem.Allocator,
    store: std.StringHashMap(obj.Object),
    outer: ?*const Environment = null,

    pub fn init(heap: std.mem.Allocator) Environment {
        return .{
            .heap = heap,
            .store = std.StringHashMap(obj.Object).init(heap),
            .outer = null,
        };
    }

    /// Create a new environment that chains to this one (for function calls / closures).
    pub fn enclose(self: *const Environment) Environment {
        return .{
            .heap = self.heap,
            .store = std.StringHashMap(obj.Object).init(self.heap),
            .outer = self,
        };
    }

    pub fn deinit(self: *Environment) void {
        self.store.deinit();
    }

    pub fn get(self: *const Environment, name: []const u8) ?obj.Object {
        if (self.store.get(name)) |value| return value;
        if (self.outer) |outer| return outer.get(name);
        return null;
    }

    pub fn set(self: *Environment, name: []const u8, value: obj.Object) !void {
        if (self.store.getEntry(name)) |entry| {
            entry.value_ptr.* = value;
        } else {
            const name_copy = try self.heap.dupe(u8, name);
            try self.store.put(name_copy, value);
        }
    }
};

pub const Evaluator = struct {
    heap: std.mem.Allocator,
    /// Global environment; never overwritten so closure pointers stay valid.
    global_env: Environment,
    /// Current environment (always points at global_env or a temporary extended env).
    env: *Environment,

    pub fn init(heap: std.mem.Allocator) Evaluator {
        var eval = Evaluator{
            .heap = heap,
            .global_env = Environment.init(heap),
            .env = undefined,
        };
        eval.env = &eval.global_env;
        eval.initBuiltins() catch {};
        return eval;
    }

    fn initBuiltins(self: *Evaluator) EvalError!void {
        const puts_obj = obj.Object{ .builtin = obj.BuiltinObject{ .@"fn" = &builtinPuts } };
        try self.env.set("puts", puts_obj);
    }

    /// Call only when heap is not an ArenaAllocator (arena free() can't handle
    /// HashMap's per-entry frees). With an arena, let the arena tear down everything.
    pub fn deinit(self: *Evaluator) void {
        self.global_env.deinit();
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
            .return_statement => |ret_stmt| {
                const value = try self.evaluateExpression(ret_stmt.return_value);
                if (value == .@"error") return value;
                const value_ptr = try self.heap.create(obj.Object);
                value_ptr.* = value;
                return obj.Object{ .@"return" = obj.ReturnValueObject{ .value = value_ptr } };
            },
            .expression_statement => |exp_stmt| {
                return try self.evaluateExpression(exp_stmt.expression);
            },
            .block_statement => |block| {
                return try self.evaluateBlockStatement(block);
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
            .boolean_literal => |bool_lit| {
                return self.evaluateBooleanLiteral(bool_lit);
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
            .function_literal => |func_lit| {
                return try self.evaluateFunctionLiteral(func_lit);
            },
            .if_expression => |if_expr| {
                return try self.evaluateIfExpression(if_expr);
            },
        };
    }

    fn evaluateBlockStatement(self: *Evaluator, block: ast.BlockStatement) EvalError!?obj.Object {
        var result: ?obj.Object = null;
        for (block.statements.items) |stmt| {
            result = try self.evaluateStatement(stmt);
            if (result) |res| {
                if (res == .@"error") return res;
                // Bubble up return value so callFunction can unwrap it
                if (res == .@"return") return res.@"return".value.*;
            }
        }
        return result;
    }

    fn evaluateIfExpression(self: *Evaluator, if_expr: ast.IfExpression) EvalError!obj.Object {
        const cond = try self.evaluateExpression(if_expr.condition.*);
        if (cond == .@"error") return cond;
        if (self.isTruthy(cond)) {
            const result = try self.evaluateBlockStatement(if_expr.consequence);
            return result orelse obj.Object{ .null = obj.NullObject{ .value = {} } };
        }
        if (if_expr.alternative) |alt| {
            const result = try self.evaluateBlockStatement(alt);
            return result orelse obj.Object{ .null = obj.NullObject{ .value = {} } };
        }
        return obj.Object{ .null = obj.NullObject{ .value = {} } };
    }

    fn isTruthy(self: *Evaluator, obj_val: obj.Object) bool {
        _ = self;
        return switch (obj_val) {
            .null => false,
            .integer => |int| int.value != 0,
            .boolean => |b| b.value,
            else => true,
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

    fn evaluateBooleanLiteral(_: *Evaluator, bool_lit: ast.BooleanLiteral) EvalError!obj.Object {
        return obj.Object{ .boolean = obj.BooleanObject{ .value = bool_lit.value } };
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
                    else => obj.Object{ .null = obj.NullObject{ .value = {} } },
                };
            },
            '-' => {
                return switch (right) {
                    .integer => |int_obj| obj.Object{ .integer = obj.IntegerObject{ .value = -int_obj.value } },
                    else => obj.Object{ .null = obj.NullObject{ .value = {} } },
                };
            },
            else => obj.Object{ .null = obj.NullObject{ .value = {} } },
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

        // + : integer addition or string concatenation (same-type operands only)
        if (std.mem.eql(u8, expr.operator, "+")) {
            if (left_obj == .integer and right_obj == .integer) {
                const left_val = left_obj.integer.value;
                const right_val = right_obj.integer.value;
                return obj.Object{ .integer = obj.IntegerObject{ .value = left_val + right_val } };
            }
            if (left_obj == .string and right_obj == .string) {
                const left_str = left_obj.string.value;
                const right_str = right_obj.string.value;
                const total_len = left_str.len + right_str.len;
                const concat = try self.heap.alloc(u8, total_len);
                @memcpy(concat[0..left_str.len], left_str);
                @memcpy(concat[left_str.len..], right_str);
                return obj.Object{ .string = obj.StringObject{ .value = concat } };
            }
            const msg = try self.heap.dupe(u8, "type mismatch: + requires both integers or both strings");
            return obj.Object{ .@"error" = obj.ErrorObject{ .message = msg } };
        }

        // Both operands must be integers for other arithmetic operations
        const left = switch (left_obj) {
            .integer => |int_obj| int_obj.value,
            else => return obj.Object{ .null = obj.NullObject{ .value = {} } },
        };
        const right = switch (right_obj) {
            .integer => |int_obj| int_obj.value,
            else => return obj.Object{ .null = obj.NullObject{ .value = {} } },
        };

        return switch (expr.operator[0]) {
            '-' => obj.Object{ .integer = obj.IntegerObject{ .value = left - right } },
            '*' => obj.Object{ .integer = obj.IntegerObject{ .value = left * right } },
            '/' => obj.Object{ .integer = obj.IntegerObject{ .value = @divTrunc(left, right) } },
            '<' => obj.Object{ .integer = obj.IntegerObject{ .value = if (left < right) 1 else 0 } },
            '>' => obj.Object{ .integer = obj.IntegerObject{ .value = if (left > right) 1 else 0 } },
            else => obj.Object{ .null = obj.NullObject{ .value = {} } },
        };
    }

    fn evaluateAssignment(self: *Evaluator, expr: ast.InfixExpression) EvalError!obj.Object {
        // Left side must be an identifier
        const left_ident = switch (expr.left.*) {
            .identifier => |ident| ident,
            else => return obj.Object{ .null = obj.NullObject{ .value = {} } },
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
            .function => |func_obj| {
                return try self.callFunction(func_obj, args.items);
            },
            else => {
                const msg = try self.heap.dupe(u8, "not a function");
                return obj.Object{ .@"error" = obj.ErrorObject{ .message = msg } };
            },
        };
    }

    fn evaluateFunctionLiteral(self: *Evaluator, func_lit: ast.FunctionLiteral) EvalError!obj.Object {
        // Extract parameter names
        const param_names = try self.heap.alloc([]const u8, func_lit.parameters.items.len);
        for (func_lit.parameters.items, 0..) |param, i| {
            param_names[i] = try self.heap.dupe(u8, param.value);
        }

        // Store body pointer
        const body_ptr = try self.heap.create(ast.BlockStatement);
        body_ptr.* = func_lit.body;

        // Create function object with pointer to current environment (closure); must store
        // the env pointer, not &self.env, so when we replace self.env during a call the
        // closure still points at the real outer env.
        return obj.Object{
            .function = obj.FunctionObject{
                .parameters = param_names,
                .body = body_ptr,
                .env = @ptrCast(self.env),
            },
        };
    }

    fn callFunction(self: *Evaluator, func_obj: obj.FunctionObject, args: []const obj.Object) EvalError!obj.Object {
        if (args.len != func_obj.parameters.len) {
            const msg = try std.fmt.allocPrint(
                self.heap,
                "wrong number of arguments: got {d}, want {d}",
                .{ args.len, func_obj.parameters.len },
            );
            return obj.Object{ .@"error" = obj.ErrorObject{ .message = msg } };
        }

        const closure_env = @as(*const Environment, @ptrCast(@alignCast(func_obj.env)));
        var extended_env = closure_env.enclose();
        defer extended_env.deinit();

        for (func_obj.parameters, args) |param_name, arg_value| {
            try extended_env.set(param_name, arg_value);
        }

        const old_env = self.env;
        self.env = &extended_env;
        defer self.env = old_env;

        const result = try self.evaluateBlockStatement(func_obj.body.*);
        if (result) |res| {
            if (res == .@"return") return res.@"return".value.*;
            return res;
        }
        return obj.Object{ .null = obj.NullObject{ .value = {} } };
    }
};
