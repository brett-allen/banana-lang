const std = @import("std");

pub const TokenType = enum {
    illegal,
    eof,
    // Identifiers + literals
    ident,
    int,
    string,
    // Operators
    assign,
    plus,
    minus,
    bang,
    asterisk,
    slash,
    lt,
    gt,
    eq,
    not_eq,
    // Delimiters
    comma,
    semicolon,
    lparen,
    rparen,
    lbrace,
    rbrace,
    lbrack,
    rbrack,
    // Keywords
    let,
    function,
    @"if",
    @"else",
    @"return",
    true,
    false,
    builtin,
};

pub const Token = struct {
    type: TokenType,
    literal: []const u8,
    line: u32 = 0,

    pub fn init(t: TokenType, lit: []const u8, line_num: u32) Token {
        return Token{
            .type = t,
            .literal = lit,
            .line = line_num,
        };
    }
};

const Keywords = [_]Token{
    Token{ .type = .let, .literal = "let" },
    Token{ .type = .function, .literal = "function" },
    Token{ .type = .function, .literal = "fn" },
    Token{ .type = .@"if", .literal = "if" },
    Token{ .type = .@"else", .literal = "else" },
    Token{ .type = .@"return", .literal = "return" },
    Token{ .type = .true, .literal = "true" },
    Token{ .type = .false, .literal = "false" },
};

pub fn lookupIdentifier(ident: []const u8) TokenType {
    for (Keywords) |keyword| {
        if (std.mem.eql(u8, ident, keyword.literal)) {
            return keyword.type;
        }
    }
    return .ident;
}
