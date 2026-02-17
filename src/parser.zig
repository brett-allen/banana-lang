const std = @import("std");
const tok = @import("token.zig");
const lex = @import("lex.zig");
const ast = @import("ast.zig");

const ParserError = error{
    MissingLetAssign,
    MissingLetIdentifier,
    MissingLeftBrace,
    MissingLeftParenthesis,
    MissingRightBrace,
    MissingRightBracket,
    MissingRightParenthesis,
    MissingColon,
    UnknownPrefixToken,
    UnknownOperatorToken,
} || std.mem.Allocator.Error || std.fmt.ParseIntError;

//
// part of the Pratt parsing magic
const Precedence = enum(u8) {
    lowest,
    equals, // == !=
    lessgreater, // > <
    sum, // + -
    product, // * /
    prefix, // -X !X
    call, // fn(x)
};

fn getPrecedence(token_type: tok.TokenType) Precedence {
    return switch (token_type) {
        .assign => Precedence.lowest, // Assignment has lowest precedence
        .eq, .not_eq => Precedence.equals,
        .lt, .gt => Precedence.lessgreater,
        .plus, .minus => Precedence.sum,
        .slash, .asterisk => Precedence.product,
        .lparen => Precedence.call,
        else => Precedence.lowest,
    };
}
pub const Parser = struct {
    lexer: lex.Lexer,
    heap: std.mem.Allocator,
    current_token: tok.Token,
    peek_token: tok.Token,

    pub fn init(heap: std.mem.Allocator, lexer: lex.Lexer) Parser {
        var p = Parser{
            .lexer = lexer,
            .heap = heap,
            .current_token = undefined,
            .peek_token = undefined,
        };

        // prime the pump
        p.nextToken();
        p.nextToken();

        return p;
    }

    pub fn parseProgram(self: *Parser) !ast.Program {
        var program = ast.Program{ .statements = std.ArrayList(ast.Statement){} };
        while (self.current_token.type != .eof) {
            const stmt = try self.parseStatement();
            if (stmt) |s| {
                try program.statements.append(self.heap, s);
            }
            self.nextToken();
        }

        return program;
    }

    fn parseStatement(self: *Parser) !?ast.Statement {
        return switch (self.current_token.type) {
            .let => ast.Statement{
                .let_statement = try self.parseLetStatement(),
            },
            else => ast.Statement{
                .expression_statement = try self.parseExpressionStatement(),
            },
        };
    }

    fn parseLetStatement(self: *Parser) !ast.LetStatement {
        const let_token = self.current_token;

        if (!self.expectPeek(.ident)) {
            return ParserError.MissingLetIdentifier;
        }

        const name = ast.Identifier{
            .token = self.current_token,
            .value = self.current_token.literal,
        };

        if (!self.expectPeek(.assign)) {
            return ParserError.MissingLetAssign;
        }

        self.nextToken();
        const value = try self.parseExpression(Precedence.lowest);

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        return ast.LetStatement{
            .token = let_token,
            .name = name,
            .value = value,
        };
    }

    fn parseExpressionStatement(self: *Parser) !ast.ExpressionStatement {
        const token = self.current_token;
        const expression = try self.parseExpression(Precedence.lowest);

        if (self.peekTokenIs(.semicolon)) {
            self.nextToken();
        }

        return ast.ExpressionStatement{
            .token = token,
            .expression = expression,
        };
    }

    fn parseExpression(self: *Parser, precedence: Precedence) !ast.Expression {
        //
        // this is the Pratt parsing magic

        //
        // we use the precedence to determine the associativity of the operator
        const p = @intFromEnum(precedence);
        const prefixFn = self.prefixFns() orelse return error.UnknownPrefixToken;
        var left = try prefixFn(self);

        while (!(self.peekTokenIs(.semicolon) or self.curTokenIs(.eof)) and p < @intFromEnum(self.peekPrecedence())) {
            const infixFn = self.infixFns() orelse return left;
            self.nextToken();

            left = try infixFn(self, left);
        }
        return left;
    }

    fn parsePrefixExpression(self: *Parser) !ast.Expression {
        const token = self.current_token;
        const precedence = self.curPrecedence();

        self.nextToken();

        const right = try self.heap.create(ast.Expression);

        right.* = try self.parseExpression(precedence);
        return ast.Expression{
            .prefix_expression = ast.PrefixExpression{
                .token = token,
                .operator = token.literal,
                .right = right,
            },
        };
    }

    fn parseInfixExpression(self: *Parser, leftExp: ast.Expression) !ast.Expression {
        const token = self.current_token;
        const precedence = self.curPrecedence();

        self.nextToken();

        const left = try self.heap.create(ast.Expression);
        left.* = leftExp;

        const right = try self.heap.create(ast.Expression);
        right.* = try self.parseExpression(precedence);

        return ast.Expression{
            .infix_expression = ast.InfixExpression{
                .token = token,
                .left = left,
                .operator = token.literal,
                .right = right,
            },
        };
    }

    //
    // literals
    fn parseIdentifier(self: *Parser) !ast.Expression {
        return ast.Expression{
            .identifier = ast.Identifier{
                .token = self.current_token,
                .value = self.current_token.literal,
            },
        };
    }

    fn parseIntegerLiteral(self: *Parser) !ast.Expression {
        const value = try std.fmt.parseInt(i64, self.current_token.literal, 10);
        return ast.Expression{
            .integer_literal = ast.IntegerLiteral{
                .token = self.current_token,
                .value = value,
            },
        };
    }

    //
    // helpers
    fn nextToken(self: *Parser) void {
        self.current_token = self.peek_token;
        self.peek_token = self.lexer.nextToken();
    }

    fn curTokenIs(self: *const Parser, t: tok.TokenType) bool {
        return self.current_token.type == t;
    }

    fn peekTokenIs(self: *const Parser, t: tok.TokenType) bool {
        return self.peek_token.type == t;
    }

    fn expectPeek(self: *Parser, token: tok.TokenType) bool {
        if (self.peekTokenIs(token)) {
            self.nextToken();
            return true;
        } else {
            return false;
        }
    }

    fn peekPrecedence(self: Parser) Precedence {
        return getPrecedence(self.peek_token.type);
    }

    /// Check the operator precedence of the current token
    fn curPrecedence(self: Parser) Precedence {
        return getPrecedence(self.current_token.type);
    }

    fn prefixFns(self: *Parser) ?*const fn (self: *Parser) ParserError!ast.Expression {
        return switch (self.current_token.type) {
            .bang, .minus => parsePrefixExpression,
            .ident => parseIdentifier,
            .int => parseIntegerLiteral,
            else => null,
        };
    }

    fn infixFns(self: *Parser) ?*const fn (self: *Parser, left: ast.Expression) ParserError!ast.Expression {
        return switch (self.peek_token.type) {
            .assign, .plus, .minus, .slash, .asterisk, .lt, .gt, .eq, .not_eq => parseInfixExpression,
            else => null,
        };
    }
};
