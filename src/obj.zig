const std = @import("std");
const ast = @import("ast.zig");

pub const ObjectError = std.Io.Writer.Error;

const ObjectType = []const u8;

pub const INTEGER_OBJ = "INTEGER";
pub const BOOLEAN_OBJ = "BOOLEAN";
pub const NULL_OBJ = "NULL";
pub const RETURN_OBJ = "RETURN_VALUE";
pub const ERROR_OBJ = "ERROR";
pub const FUNCTION_OBJ = "FUNCTION";
pub const STRING_OBJ = "STRING";
pub const BUILTIN_OBJ = "BUILTIN";
pub const ARRAY_OBJ = "ARRAY";
pub const HASH_OBJ = "HASH";

// BuiltinFunction type - will be properly typed in evaluator.zig
pub const BuiltinFunction = *const fn (allocator: std.mem.Allocator, args: []const Object) anyerror!Object;

pub const Object = union(enum) {
    integer: IntegerObject,
    boolean: BooleanObject,
    @"null": NullObject,
    @"return": ReturnValueObject,
    @"error": ErrorObject,
    builtin: BuiltinObject,
    string: StringObject,
    function: FunctionObject,
    array: ArrayObject,
    hash: HashObject,

    pub fn _type(self: Object) ObjectType {
        return switch (self) {
            .integer => INTEGER_OBJ,
            .boolean => BOOLEAN_OBJ,
            .@"null" => NULL_OBJ,
            .@"return" => RETURN_OBJ,
            .@"error" => ERROR_OBJ,
            .function => FUNCTION_OBJ,
            .string => STRING_OBJ,
            .builtin => BUILTIN_OBJ,
            .array => ARRAY_OBJ,
            .hash => HASH_OBJ,
        };
    }

    pub fn inspect(self: Object, writer: *std.Io.Writer) ObjectError!void {
        switch (self) {
            inline else => |obj| try obj.inspect(writer),
        }
    }
};

pub const IntegerObject = struct {
    value: i64,

    pub fn inspect(self: *const IntegerObject, writer: *std.Io.Writer) ObjectError!void {
        return try writer.print("{d}", .{self.value});
    }

    pub fn dupe(self: IntegerObject, _: std.mem.Allocator) std.mem.Allocator.Error!Object {
        return Object{ .integer = self };
    }

    pub fn hash(self: *const IntegerObject, hasher: *std.hash.Fnv1a_64) void {
        hasher.update(std.mem.asBytes(&Object.integer));
        hasher.update(std.mem.asBytes(&self.value));
    }

    pub fn eql_hash(self: IntegerObject, other: Object) bool {
        return switch (other) {
            .integer => |int| self.value == int.value,
            else => false,
        };
    }
};

pub const BooleanObject = struct {
    value: bool,

    pub fn inspect(self: *const BooleanObject, writer: *std.Io.Writer) ObjectError!void {
        return try writer.print("{}", .{self.value});
    }
};

pub const NullObject = struct {
    value: void,

    pub fn inspect(_: *const NullObject, writer: *std.Io.Writer) ObjectError!void {
        return try writer.writeAll("null");
    }
};

pub const ReturnValueObject = struct {
    /// Pointer to avoid Object union depending on itself (ReturnValueObject -> Object -> ReturnValueObject).
    value: *const Object,

    pub fn inspect(self: *const ReturnValueObject, writer: *std.Io.Writer) ObjectError!void {
        return self.value.inspect(writer);
    }
};

pub const ErrorObject = struct {
    message: []const u8,

    pub fn inspect(self: *const ErrorObject, writer: *std.Io.Writer) ObjectError!void {
        return try writer.writeAll(self.message);
    }
};

pub const BuiltinObject = struct {
    @"fn": BuiltinFunction,

    pub fn inspect(_: *const BuiltinObject, writer: *std.Io.Writer) ObjectError!void {
        return try writer.writeAll("builtin function");
    }
};

pub const StringObject = struct {
    value: []const u8,

    pub fn inspect(self: *const StringObject, writer: *std.Io.Writer) ObjectError!void {
        return try writer.writeAll(self.value);
    }
};

/// Array elements stored as an arena-allocated slice (no separate allocator to store).
pub const ArrayObject = struct {
    elements: []const Object,

    pub fn inspect(self: *const ArrayObject, writer: *std.Io.Writer) ObjectError!void {
        try writer.writeByte('[');
        for (self.elements, 0..) |elem, i| {
            try elem.inspect(writer);
            if (i < self.elements.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeByte(']');
    }
};

/// Hash: pairs of (key, value). Keys must be hashable (integer, string, boolean). Arena-allocated slice.
pub const HashPair = struct {
    key: Object,
    value: Object,
};

pub const HashObject = struct {
    pairs: []const HashPair,

    pub fn inspect(self: *const HashObject, writer: *std.Io.Writer) ObjectError!void {
        try writer.writeByte('{');
        for (self.pairs, 0..) |pair, i| {
            try pair.key.inspect(writer);
            try writer.writeAll(": ");
            try pair.value.inspect(writer);
            if (i < self.pairs.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeByte('}');
    }
};

/// Returns true if the object is a valid hash key type (integer, string, boolean).
pub fn isHashKey(obj: Object) bool {
    return switch (obj) {
        .integer, .string, .boolean => true,
        else => false,
    };
}

/// Equality for hash keys (same type and value).
pub fn hashKeyEql(a: Object, b: Object) bool {
    if (a != .integer and a != .string and a != .boolean) return false;
    if (b != .integer and b != .string and b != .boolean) return false;
    return switch (a) {
        .integer => |av| switch (b) {
            .integer => |bv| av.value == bv.value,
            else => false,
        },
        .boolean => |av| switch (b) {
            .boolean => |bv| av.value == bv.value,
            else => false,
        },
        .string => |av| switch (b) {
            .string => |bv| std.mem.eql(u8, av.value, bv.value),
            else => false,
        },
        else => false,
    };
}

// Environment is defined in evaluator.zig - we'll use a pointer type
pub const FunctionObject = struct {
    parameters: []const []const u8, // Parameter names
    body: *const ast.BlockStatement, // Function body
    env: *anyopaque, // Closure environment (will be cast to *Environment in evaluator)

    pub fn inspect(self: *const FunctionObject, writer: *std.Io.Writer) ObjectError!void {
        try writer.writeAll("fn(");
        for (self.parameters, 0..) |param, i| {
            try writer.writeAll(param);
            if (i < self.parameters.len - 1) {
                try writer.writeAll(", ");
            }
        }
        try writer.writeAll(") {\n");
        // TODO: print body
        try writer.writeAll("}");
    }
};
