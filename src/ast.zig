const std = @import("std");
const tok = @import("token.zig");

const Node = enum { Program, Statement, Expression };

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
    prefix_expression: PrefixExpression,
    infix_expression: InfixExpression,

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
        try writer.writeAll(self.operator);
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
