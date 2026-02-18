const std = @import("std");
const token = @import("token.zig");

const T = token.Token.init;

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,

    pub fn init(input: []const u8) Lexer {
        var l = Lexer{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = 0,
        };
        l.readChar();
        return l;
    }

    pub fn nextToken(self: *Lexer) token.Token {
        self.skipWhitespace();
        var tok = token.Token.init(token.TokenType.illegal, "");

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    const start = self.position;
                    self.readChar(); // consume the second '='
                    tok = token.Token.init(token.TokenType.eq, self.input[start..self.read_position]);
                } else {
                    tok = token.Token.init(token.TokenType.assign, "=");
                }
            },
            ';' => tok = token.Token.init(token.TokenType.semicolon, ";"),
            ',' => tok = token.Token.init(token.TokenType.comma, ","),
            '(' => tok = token.Token.init(token.TokenType.lparen, "("),
            ')' => tok = token.Token.init(token.TokenType.rparen, ")"),
            '{' => tok = token.Token.init(token.TokenType.lbrace, "{"),
            '}' => tok = token.Token.init(token.TokenType.rbrace, "}"),
            '[' => tok = token.Token.init(token.TokenType.lbrack, "["),
            ']' => tok = token.Token.init(token.TokenType.rbrack, "]"),
            '+' => tok = token.Token.init(token.TokenType.plus, "+"),
            '-' => tok = token.Token.init(token.TokenType.minus, "-"),
            '*' => tok = token.Token.init(token.TokenType.asterisk, "*"),
            '/' => tok = token.Token.init(token.TokenType.slash, "/"),
            '<' => tok = token.Token.init(token.TokenType.lt, "<"),
            '>' => tok = token.Token.init(token.TokenType.gt, ">"),
            '!' => {
                if (self.peekChar() == '=') {
                    const start = self.position;
                    self.readChar(); // consume the '='
                    tok = token.Token.init(token.TokenType.not_eq, self.input[start..self.read_position]);
                } else {
                    tok = token.Token.init(token.TokenType.bang, "!");
                }
            },
            '"' => {
                tok = T(.string, self.readString());
                return tok;
            },
            0 => tok = token.Token.init(token.TokenType.eof, ""),
            else => {
                if (self.isLetter()) {
                    const literal = self.readIdentifier();
                    tok = T(token.lookupIdentifier(literal), literal);
                    return tok;
                } else if (self.isDigit()) {
                    tok = T(.int, self.readNumber());
                    return tok;
                } else {
                    tok = T(.illegal, self.input[self.position .. self.position + 1]);
                }
            },
        }

        self.readChar();
        return tok;
    }

    fn readIdentifier(self: *Lexer) []const u8 {
        const start = self.position;
        while (self.isLetter()) {
            self.readChar();
        }
        return self.input[start..self.position];
    }

    fn readChar(self: *Lexer) void {
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
    }

    fn readNumber(self: *Lexer) []const u8 {
        const start = self.position;
        while (self.isDigit()) {
            self.readChar();
        }
        return self.input[start..self.position];
    }

    fn readString(self: *Lexer) []const u8 {
        self.readChar(); // Skip opening quote
        const start = self.position;
        while (self.ch != '"' and self.ch != 0) {
            self.readChar();
        }
        const value = self.input[start..self.position];
        if (self.ch == '"') {
            self.readChar(); // Skip closing quote
        }
        return value;
    }

    fn isLetter(self: *Lexer) bool {
        return std.ascii.isAlphabetic(self.ch) or self.ch == '_';
    }

    fn isDigit(self: *Lexer) bool {
        return std.ascii.isDigit(self.ch);
    }

    fn skipWhitespace(self: *Lexer) void {
        while (std.ascii.isWhitespace(self.ch)) {
            self.readChar();
        }
    }

    fn peekChar(self: *const Lexer) u8 {
        if (self.read_position >= self.input.len) {
            return 0;
        } else {
            return self.input[self.read_position];
        }
    }
};
