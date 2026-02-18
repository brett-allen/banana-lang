const std = @import("std");
const tok = @import("token.zig");

pub const Node = union(enum) {
    program: Program,
    statement: Statement,
    expression: Expression,

    pub fn tokenLiteral(self: Node) []const u8 {
        return switch (self) {
            inline else => |node| node.tokenLiteral(),
        };
    }

    pub fn string(self: Node, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            inline else => |node| node.string(writer),
        }
    }
};

pub const Program = struct {
    statements: std.ArrayList(Statement),

    pub fn tokenLiteral(self: *const Program) []const u8 {
        if (self.statements.len > 0) {
            return self.statements[0].tokenLiteral();
        } else {
            return "";
        }
    }

    pub fn string(self: *const Program, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        for (self.statements.items) |stmt| {
            try stmt.string(writer);
            try writer.writeByte('\n');
        }
    }
};

pub const Statement = union(enum) {
    let_statement: LetStatement,
    expression_statement: ExpressionStatement,

    pub fn string(self: Statement, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self) {
            inline else => |stmt| try stmt.string(writer),
        }
    }
};

pub const LetStatement = struct {
    token: tok.Token,
    name: Identifier,
    value: Expression,

    pub fn tokenLiteral(self: *const LetStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const LetStatement, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeByte(' ');

        try self.name.string(writer);

        try writer.writeAll(" = ");
        try self.value.string(writer);

        try writer.writeByte(';');
    }
};

pub const Expression = union(enum) {
    identifier: Identifier,
    integer_literal: IntegerLiteral,
    string_literal: StringLiteral,
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,
    call_expression: CallExpression,
    function_literal: FunctionLiteral,

    pub fn string(self: *const Expression, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        switch (self.*) {
            inline else => |exp| try exp.string(writer),
        }
    }
};

pub const ExpressionStatement = struct {
    token: tok.Token,
    expression: Expression,

    pub fn tokenLiteral(self: *const ExpressionStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const ExpressionStatement, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try self.expression.string(writer);
        try writer.writeByte(';');
    }
};

pub const PrefixExpression = struct {
    token: tok.Token,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: *const PrefixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const PrefixExpression, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.tokenLiteral());
        try self.right.string(writer);
    }
};

pub const InfixExpression = struct {
    token: tok.Token,
    left: *Expression,
    operator: []const u8,
    right: *Expression,

    pub fn tokenLiteral(self: *const InfixExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const InfixExpression, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try self.left.string(writer);
        try writer.print(" {s} ", .{self.operator});
        try self.right.string(writer);
    }
};

pub const Identifier = struct {
    token: tok.Token,
    value: []const u8,

    pub fn tokenLiteral(self: *const Identifier) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const Identifier, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.value);
    }

    pub fn dupe(self: *const Identifier, alloc: std.mem.Allocator) !Identifier {
        return Identifier{
            .token = self.token,
            .value = try std.mem.dupe(alloc, u8, self.value),
        };
    }

    pub fn hash(self: *const Identifier, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&self.token.token_type));
        hasher.update(self.value);
    }

    pub fn eql(self: *const Identifier, other: *const Expression) bool {
        switch (other.*) {
            .identifier => |ident| return std.mem.eql(u8, self.value, ident.value),
            else => return false,
        }
    }
};

pub const IntegerLiteral = struct {
    token: tok.Token,
    value: i64,

    pub fn tokenLiteral(self: *const IntegerLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const IntegerLiteral, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.tokenLiteral());
    }
};

pub const StringLiteral = struct {
    token: tok.Token,
    value: []const u8,

    pub fn tokenLiteral(self: *const StringLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const StringLiteral, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeByte('"');
        try writer.writeAll(self.value);
        try writer.writeByte('"');
    }
};

pub const CallExpression = struct {
    token: tok.Token,
    function: *Expression,
    arguments: std.ArrayList(Expression),

    pub fn tokenLiteral(self: *const CallExpression) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const CallExpression, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try self.function.string(writer);
        try writer.writeByte('(');
        for (self.arguments.items, 0..) |arg, i| {
            try arg.string(writer);
            if (i < self.arguments.items.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeByte(')');
    }
};

pub const BlockStatement = struct {
    token: tok.Token,
    statements: std.ArrayList(Statement),

    pub fn tokenLiteral(self: *const BlockStatement) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const BlockStatement, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        for (self.statements.items) |stmt| {
            try stmt.string(writer);
        }
    }
};

pub const FunctionLiteral = struct {
    token: tok.Token,
    parameters: std.ArrayList(Identifier),
    body: BlockStatement,

    pub fn tokenLiteral(self: *const FunctionLiteral) []const u8 {
        return self.token.literal;
    }

    pub fn string(self: *const FunctionLiteral, writer: *std.Io.Writer) std.Io.Writer.Error!void {
        try writer.writeAll(self.tokenLiteral());
        try writer.writeByte('(');
        for (self.parameters.items, 0..) |param, i| {
            try param.string(writer);
            if (i < self.parameters.items.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(") {\n");
        try self.body.string(writer);
        try writer.writeByte('\n');
        try writer.writeByte('}');
    }
};
