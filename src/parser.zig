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

/// Returns a short, user-friendly message for a parse error (no stack trace).
pub fn parseErrorMessage(err: ParserError) []const u8 {
    return switch (err) {
        ParserError.MissingLetAssign => "expected '=' after identifier in let",
        ParserError.MissingLetIdentifier => "expected identifier after let",
        ParserError.MissingLeftBrace => "expected '{'",
        ParserError.MissingLeftParenthesis => "expected '('",
        ParserError.MissingRightBrace => "expected '}'",
        ParserError.MissingRightBracket => "expected ']'",
        ParserError.MissingRightParenthesis => "expected ')'",
        ParserError.MissingColon => "expected ':'",
        ParserError.UnknownPrefixToken => "unexpected token (invalid expression)",
        ParserError.UnknownOperatorToken => "unexpected operator",
        else => "parse error",
    };
}

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

    fn parseStatement(self: *Parser) ParserError!?ast.Statement {
        // If we're on a closing brace or semicolon, there's no statement to parse
        if (self.curTokenIs(.rbrace) or self.curTokenIs(.semicolon) or self.curTokenIs(.eof)) {
            return null;
        }
        return switch (self.current_token.type) {
            .let => ast.Statement{
                .let_statement = try self.parseLetStatement(),
            },
            .lbrace => ast.Statement{
                .block_statement = try self.parseBlockStatement(),
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
            const infixFn = self.infixFns() orelse break;
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

    fn parseGroupedExpression(self: *Parser) ParserError!ast.Expression {
        self.nextToken();

        const exp = try self.parseExpression(Precedence.lowest);

        if (!self.expectPeek(.rparen)) {
            return ParserError.MissingRightParenthesis;
        }

        return exp;
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

    fn parseStringLiteral(self: *Parser) !ast.Expression {
        return ast.Expression{
            .string_literal = ast.StringLiteral{
                .token = self.current_token,
                .value = self.current_token.literal,
            },
        };
    }

    fn parseBooleanLiteral(self: *const Parser) !ast.Expression {
        return ast.Expression{
            .boolean_literal = ast.BooleanLiteral{
                .token = self.current_token,
                .value = self.curTokenIs(.true),
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
            .ident => parseIdentifier,
            .int => parseIntegerLiteral,
            .bang, .minus => parsePrefixExpression,
            .string => parseStringLiteral,
            .true, .false => parseBooleanLiteral,
            .lparen => parseGroupedExpression,
            .function => parseFunctionLiteral,
            .@"if" => parseIfExpression,
            else => null,
        };
    }

    fn infixFns(self: *Parser) ?*const fn (self: *Parser, left: ast.Expression) ParserError!ast.Expression {
        return switch (self.peek_token.type) {
            .assign, .plus, .minus, .slash, .asterisk, .lt, .gt, .eq, .not_eq => parseInfixExpression,
            .lparen => parseCallExpression,
            else => null,
        };
    }

    fn parseCallExpression(self: *Parser, function: ast.Expression) !ast.Expression {
        const token = self.current_token;
        const args = try self.parseExpressionList(.rparen);

        const function_ptr = try self.heap.create(ast.Expression);
        function_ptr.* = function;

        return ast.Expression{
            .call_expression = ast.CallExpression{
                .token = token,
                .function = function_ptr,
                .arguments = args,
            },
        };
    }

    fn parseExpressionList(self: *Parser, end: tok.TokenType) !std.ArrayList(ast.Expression) {
        var list = std.ArrayList(ast.Expression){};

        if (self.peekTokenIs(end)) {
            self.nextToken();
            return list;
        }

        self.nextToken();
        try list.append(self.heap, try self.parseExpression(Precedence.lowest));

        while (self.peekTokenIs(.comma)) {
            self.nextToken();
            self.nextToken();
            try list.append(self.heap, try self.parseExpression(Precedence.lowest));
        }

        if (!self.expectPeek(end)) {
            return ParserError.MissingRightParenthesis;
        }

        return list;
    }

    fn parseFunctionLiteral(self: *Parser) !ast.Expression {
        const token = self.current_token;

        if (!self.expectPeek(.lparen)) {
            return ParserError.MissingLeftParenthesis;
        }

        const params = try self.parseFunctionParameters();

        if (!self.expectPeek(.lbrace)) {
            return ParserError.MissingLeftBrace;
        }

        const body = try self.parseBlockStatement();
        // Leave current_token on '}' so parseLetStatement's caller sees it and nextToken() gets to next statement
        return ast.Expression{
            .function_literal = ast.FunctionLiteral{
                .token = token,
                .parameters = params,
                .body = body,
            },
        };
    }

    fn parseIfExpression(self: *Parser) !ast.Expression {
        // Save the 'if' token
        const token = self.current_token;
        if (!self.expectPeek(.lparen)) {
            return ParserError.MissingLeftParenthesis;
        }
        //
        // Advance past the '(' to get to the condition
        self.nextToken();
        //
        // Parse the condition
        const condition = try self.heap.create(ast.Expression);
        condition.* = try self.parseExpression(Precedence.lowest);
        if (!self.expectPeek(.rparen)) {
            return ParserError.MissingRightParenthesis;
        }
        //
        // Parse the consequence
        if (!self.expectPeek(.lbrace)) {
            return ParserError.MissingLeftBrace;
        }

        const consequence = try self.parseBlockStatement();
        if (self.curTokenIs(.rbrace)) {
            self.nextToken(); // advance past '}'
        }
        //
        // Parse the else alternative
        var alternative: ?ast.BlockStatement = null;

        if (self.curTokenIs(.@"else")) {
            self.nextToken(); // consume 'else', now current_token is '{'
            if (!self.curTokenIs(.lbrace)) {
                return ParserError.MissingLeftBrace;
            }
            alternative = try self.parseBlockStatement();
        }

        return ast.Expression{
            .if_expression = ast.IfExpression{
                .token = token,
                .condition = condition,
                .consequence = consequence,
                .alternative = alternative,
            },
        };
    }

    fn parseFunctionParameters(self: *Parser) !std.ArrayList(ast.Identifier) {
        var identifiers = std.ArrayList(ast.Identifier){};

        if (self.peekTokenIs(.rparen)) {
            self.nextToken();
            return identifiers;
        }

        self.nextToken();

        var ident = ast.Identifier{
            .token = self.current_token,
            .value = self.current_token.literal,
        };
        try identifiers.append(self.heap, ident);

        while (self.peekTokenIs(.comma)) {
            self.nextToken();
            self.nextToken();
            ident = ast.Identifier{
                .token = self.current_token,
                .value = self.current_token.literal,
            };
            try identifiers.append(self.heap, ident);
        }

        if (!self.expectPeek(.rparen)) {
            return ParserError.MissingRightParenthesis;
        }

        return identifiers;
    }

    fn parseBlockStatement(self: *Parser) !ast.BlockStatement {
        const token = self.current_token; // This should be the '{' token
        var statements = std.ArrayList(ast.Statement){};

        // expectPeek(.lbrace) already advanced us past '{', so current_token is first token inside
        // But wait - let me check: expectPeek checks peek_token, so if peek was '{',
        // then after expectPeek, current_token is '{' and peek_token is first token inside
        // So we need to advance past '{'
        self.nextToken();

        while (!self.curTokenIs(.rbrace) and !self.curTokenIs(.eof)) {
            // Skip semicolons - they're statement terminators
            if (self.curTokenIs(.semicolon)) {
                self.nextToken();
                continue;
            }
            if (try self.parseStatement()) |stmt| {
                try statements.append(self.heap, stmt);
                // Advance past the last token of the statement so we don't re-parse it
                self.nextToken();
            }
        }

        // Leave current_token on '}' so callers advance correctly (avoid skipping next statement)
        if (!self.curTokenIs(.rbrace)) {
            return ParserError.MissingRightBrace;
        }

        return ast.BlockStatement{
            .token = token,
            .statements = statements,
        };
    }
};
