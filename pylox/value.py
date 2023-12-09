from dataclasses import dataclass
import enum
from types import NoneType
from typing import Callable, TypeGuard


from .shared import printf


Nil = NoneType
Bool = bool
Number = float


@dataclass
class Obj:
    pass


@dataclass
class ObjString(Obj):
    value: str

    def __hash__(self) -> int:
        return self.value.__hash__()


@dataclass
class Chunk:
    code: bytearray
    constants: "ValueArray"
    lines: list[int]


def init_chunk() -> Chunk:
    return Chunk(
        code=bytearray(),
        constants=init_value_array(),
        lines=[],
    )


@dataclass
class ObjFunction(Obj):
    arity: int
    chunk: Chunk
    name: ObjString
    upvalue_count: int


def new_function() -> ObjFunction:
    function = ObjFunction(
        arity=0, chunk=init_chunk(), name=ObjString("NONE"), upvalue_count=0
    )
    return function


NativeFn = Callable[[int, "Value"], "Value"]


@dataclass
class ObjNative(Obj):
    function: NativeFn


def new_native(function: NativeFn) -> ObjNative:
    return ObjNative(function=function)


@dataclass
class ObjClosure(Obj):
    function: ObjFunction
    upvalues: "list[ObjUpvalue]"
    upvalue_count: int


def new_closure(function: ObjFunction) -> ObjClosure:
    upvalue_count = function.upvalue_count
    return ObjClosure(
        function=function,
        upvalues=[new_upvalue(nil_val()) for _ in range(upvalue_count)],
        upvalue_count=upvalue_count,
    )


@dataclass
class ObjClass(Obj):
    name: ObjString
    methods: dict[ObjString, "Value"]


def new_class(name: ObjString):
    return ObjClass(name, methods={})


@dataclass
class ObjInstance(Obj):
    klass: ObjClass
    fields: dict[ObjString, "Value"]


def new_instance(klass: ObjClass) -> ObjInstance:
    return ObjInstance(klass=klass, fields={})


@dataclass
class ObjUpvalue(Obj):
    location: "Value"
    next: "ObjUpvalue|None"
    closed: "Value|None"


def new_upvalue(slot: "Value") -> ObjUpvalue:
    return ObjUpvalue(location=slot, next=None, closed=None)


@dataclass
class ObjBoundMethod(Obj):
    receiver: "Value"
    method: ObjClosure


def new_bound_method(receiver: "Value", method: ObjClosure):
    return ObjBoundMethod(receiver, method)


Value = Bool | Nil | Number | Obj


def bool_val(value: bool) -> Value:
    return value


def nil_val() -> Value:
    return None


def number_val(value: float) -> Value:
    return value


def obj_val(value: Obj) -> Value:
    return value


def as_bool(value: Value) -> Bool:
    if not is_bool(value):
        raise Exception("not bool", value)
    return value


def as_nil(value: Value) -> None:
    if not is_nil(value):
        raise Exception("not Nil", value)
    return value


def as_string(value: Value) -> ObjString:
    if not is_string(value):
        raise Exception("not string", value)
    return value


def as_number(value: Value) -> Number:
    if not is_number(value):
        raise Exception("not float", value)
    return value


def as_function(value: Value) -> ObjFunction:
    if not is_function(value):
        raise Exception("not function", value)
    return value


def as_class(value: Value) -> ObjClass:
    if not is_class(value):
        raise Exception("not class", value)
    return value


def as_instance(value: Value) -> ObjInstance:
    if not is_instance(value):
        raise Exception("not instance", value)
    return value


def as_bound_method(value: Value) -> ObjBoundMethod:
    if not is_bound_method(value):
        raise Exception("not instance", value)
    return value


def as_closure(value: Value) -> ObjClosure:
    if not is_closure(value):
        raise Exception("not closure", value)
    return value


def is_bool(value: Value) -> TypeGuard[Bool]:
    return isinstance(value, Bool)


def is_nil(value: Value) -> TypeGuard[Nil]:
    return isinstance(value, Nil)


def is_number(value: Value) -> TypeGuard[Number]:
    return isinstance(value, Number)


def is_obj(value: Value) -> TypeGuard[Obj]:
    return isinstance(value, Obj)


def is_string(value: Value) -> TypeGuard[ObjString]:
    return isinstance(value, ObjString)


def is_function(value: Value) -> TypeGuard[ObjFunction]:
    return isinstance(value, ObjFunction)


def is_native(value: Value) -> TypeGuard[ObjNative]:
    return isinstance(value, ObjNative)


def is_closure(value: Value) -> TypeGuard[ObjClosure]:
    return isinstance(value, ObjClosure)


def is_upvalue(value: Value) -> TypeGuard[ObjUpvalue]:
    return isinstance(value, ObjUpvalue)


def is_class(value: Value) -> TypeGuard[ObjClass]:
    return isinstance(value, ObjClass)


def is_instance(value: Value) -> TypeGuard[ObjInstance]:
    return isinstance(value, ObjInstance)


def is_bound_method(value: Value) -> TypeGuard[ObjBoundMethod]:
    return isinstance(value, ObjBoundMethod)


def values_equal(a: Value, b: Value) -> bool:
    return a == b


ValueArray = list[Value]


def init_value_array() -> ValueArray:
    return ValueArray()


def write_value_array(array: ValueArray, value: Value):
    array.append(value)


def print_value(value: Value):
    if is_nil(value):
        printf("nil")
    elif is_bool(value):
        printf("true" if value else "false")
    elif is_number(value):
        printf("{0:g}", value)
    elif is_obj(value):
        print_object(value)
    else:
        raise Exception("Wrong value type", type(value), value)


def print_object(value: Obj):
    if is_string(value):
        printf("{0:s}", value.value)
    elif is_function(value):
        printf("<fn {0:s}>", value.name.value)
    elif is_native(value):
        printf("<native fn>")
    elif is_closure(value):
        printf("<closure fn>")
    elif is_upvalue(value):
        printf("<closure fn>")
    elif is_class(value):
        printf("<class {0:s}>", value.name.value)
    elif is_instance(value):
        printf("instance {0:s}", value.klass.name.value)
    elif is_bound_method(value):
        printf("instance {0:s}", value.method.function.name.value)
    else:
        raise Exception("Wrong value type", type(value), value)


byte = int


def write_chunk(chunk: Chunk, b: byte, line: int):
    chunk.code.append(b)
    chunk.lines.append(line)


def add_constant(chunk: Chunk, value: Value) -> int:
    write_value_array(chunk.constants, value)
    return len(chunk.constants) - 1


class Op(byte, enum.Enum):
    CONSTANT = enum.auto()

    NIL = enum.auto()
    TRUE = enum.auto()
    FALSE = enum.auto()

    EQUAL = enum.auto()
    GREATER = enum.auto()
    LESS = enum.auto()

    ADD = enum.auto()
    SUBTRACT = enum.auto()
    MULTIPLY = enum.auto()
    DIVIDE = enum.auto()

    NOT = enum.auto()

    NEGATE = enum.auto()

    PRINT = enum.auto()
    ASSERT = enum.auto()

    POP = enum.auto()

    DEFINE_GLOBAL = enum.auto()
    SET_GLOBAL = enum.auto()
    GET_GLOBAL = enum.auto()

    SET_LOCAL = enum.auto()
    GET_LOCAL = enum.auto()

    GET_UPVALUE = enum.auto()
    SET_UPVALUE = enum.auto()
    CLOSE_UPVALUE = enum.auto()

    JUMP = enum.auto()
    JUMP_IF_FALSE = enum.auto()
    LOOP = enum.auto()

    CALL = enum.auto()

    CLOSURE = enum.auto()

    CLASS = enum.auto()
    METHOD = enum.auto()
    INVOKE = enum.auto()
    INHERIT = enum.auto()
    SUPER_INVOKE = enum.auto()
    GET_SUPER = enum.auto()

    SET_PROPERTY = enum.auto()
    GET_PROPERTY = enum.auto()

    RETURN = enum.auto()
