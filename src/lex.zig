const std = @import("std");
const token = @import("token.zig");

const T = token.Token.init;

pub const Lexer = struct {
    input: []const u8,
    position: usize,
    read_position: usize,
    ch: u8,
    line: u32,
    column: u32,

    pub fn init(input: []const u8) Lexer {
        var l = Lexer{
            .input = input,
            .position = 0,
            .read_position = 0,
            .ch = 0,
            .line = 1,
            .column = 1,
        };
        l.readChar();
        return l;
    }

    pub fn nextToken(self: *Lexer) token.Token {
        self.skipWhitespace();
        const line = self.line;
        var tok = token.Token.init(token.TokenType.illegal, "", line);

        switch (self.ch) {
            '=' => {
                if (self.peekChar() == '=') {
                    const start = self.position;
                    self.readChar(); // consume the second '='
                    tok = token.Token.init(token.TokenType.eq, self.input[start..self.read_position], line);
                } else {
                    tok = token.Token.init(token.TokenType.assign, "=", line);
                }
            },
            ';' => tok = token.Token.init(token.TokenType.semicolon, ";", line),
            ':' => tok = token.Token.init(token.TokenType.colon, ":", line),
            ',' => tok = token.Token.init(token.TokenType.comma, ",", line),
            '(' => tok = token.Token.init(token.TokenType.lparen, "(", line),
            ')' => tok = token.Token.init(token.TokenType.rparen, ")", line),
            '{' => tok = token.Token.init(token.TokenType.lbrace, "{", line),
            '}' => tok = token.Token.init(token.TokenType.rbrace, "}", line),
            '[' => tok = token.Token.init(token.TokenType.lbrack, "[", line),
            ']' => tok = token.Token.init(token.TokenType.rbrack, "]", line),
            '+' => tok = token.Token.init(token.TokenType.plus, "+", line),
            '-' => tok = token.Token.init(token.TokenType.minus, "-", line),
            '*' => tok = token.Token.init(token.TokenType.asterisk, "*", line),
            '/' => tok = token.Token.init(token.TokenType.slash, "/", line),
            '<' => tok = token.Token.init(token.TokenType.lt, "<", line),
            '>' => tok = token.Token.init(token.TokenType.gt, ">", line),
            '!' => {
                if (self.peekChar() == '=') {
                    const start = self.position;
                    self.readChar(); // consume the '='
                    tok = token.Token.init(token.TokenType.not_eq, self.input[start..self.read_position], line);
                } else {
                    tok = token.Token.init(token.TokenType.bang, "!", line);
                }
            },
            '"' => {
                tok = token.Token.init(token.TokenType.string, self.readString(), line);
                return tok;
            },
            0 => tok = token.Token.init(token.TokenType.eof, "", line),
            else => {
                if (self.isLetter()) {
                    const literal = self.readIdentifier();
                    tok = token.Token.init(token.lookupIdentifier(literal), literal, line);
                    return tok;
                } else if (self.isDigit()) {
                    tok = token.Token.init(token.TokenType.int, self.readNumber(), line);
                    return tok;
                } else {
                    tok = token.Token.init(token.TokenType.illegal, self.input[self.position .. self.position + 1], line);
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
        const old_pos = self.position;
        if (self.read_position >= self.input.len) {
            self.ch = 0;
        } else {
            self.ch = self.input[self.read_position];
        }
        self.position = self.read_position;
        self.read_position += 1;
        if (old_pos < self.input.len and self.input[old_pos] == '\n') {
            self.line += 1;
            self.column = 1;
        } else {
            self.column += 1;
        }
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
