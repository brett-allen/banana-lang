const std = @import("std");

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
    @"error": ErrorObject,
    builtin: BuiltinObject,
    string: StringObject,

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
