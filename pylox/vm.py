from dataclasses import dataclass
import operator
from typing import Any, Callable
import time

from .compiler import compile_source
from .debug import disassemble_instruction
from .shared import printf, printf_err
from .value import (
    Chunk,
    NativeFn,
    Number,
    ObjClass,
    ObjClosure,
    ObjString,
    ObjUpvalue,
    Value,
    as_bound_method,
    as_class,
    as_closure,
    as_function,
    as_instance,
    as_string,
    bool_val,
    init_chunk,
    is_bool,
    is_bound_method,
    is_class,
    is_closure,
    is_instance,
    is_native,
    is_nil,
    is_number,
    is_string,
    new_bound_method,
    new_class,
    new_closure,
    new_instance,
    new_native,
    new_upvalue,
    nil_val,
    print_value,
    values_equal,
)
from .value import Op


_debug_trace_execution = False


def set_debug_trace_execution(b: bool):
    global _debug_trace_execution
    _debug_trace_execution = b


@dataclass
class CallFrame:
    closure: ObjClosure
    ip: int
    stack_top: int
    slots: list[Value]


@dataclass
class VM:
    chunk: Chunk
    stack: list[Value]
    stack_top: int
    globals: dict[ObjString, Value]
    frames: list[CallFrame]
    open_upvalues: ObjUpvalue | None
    init_string: ObjString


@dataclass(frozen=True)
class InterpretOk:
    pass


@dataclass(frozen=True)
class CompileError:
    pass


@dataclass(frozen=True)
class InterpretError:
    pass


@dataclass(frozen=True)
class InterpretAssertionError(InterpretError):
    pass


InterpretResult = InterpretOk | CompileError | InterpretError


vm: VM


def init_vm():
    global vm
    vm = VM(
        chunk=init_chunk(),
        stack_top=0,
        stack=[nil_val() for _ in range(256)],
        globals={},
        frames=[],
        open_upvalues=None,
        init_string=ObjString("init"),
    )

    define_native("clock", clock_native)


def interpret(source: str) -> InterpretResult:
    function = compile_source(source)
    if function is None:
        return CompileError()

    push(function)
    closure = new_closure(function)
    pop()
    push(closure)
    call(closure, 0)

    result = run()
    return result


def run() -> InterpretResult:
    frame: CallFrame = vm.frames[-1]
    while True:
        assert frame.closure.function.chunk.code[-1] == Op.RETURN

        if _debug_trace_execution:
            printf("          ")
            for slot in range(vm.stack_top):
                printf("[ ")
                print_value(vm.stack[slot])
                printf(" ]")
            printf("\n")

            disassemble_instruction(frame.closure.function.chunk, frame.ip)

        instruction = read_byte(frame)
        match instruction:
            case Op.CONSTANT:
                constant = read_constant(frame)
                push(constant)

            case Op.NIL:
                push(nil_val())
            case Op.TRUE:
                push(bool_val(True))
            case Op.FALSE:
                push(bool_val(False))

            case Op.EQUAL:
                b = pop()
                a = pop()
                push(values_equal(a, b))
            case Op.GREATER:
                binary_op(operator.gt)
            case Op.LESS:
                binary_op(operator.lt)

            case Op.ADD:
                if concatenate():
                    pass
                elif binary_op(operator.add):
                    pass
                else:
                    return add_error()
            case Op.SUBTRACT:
                if not binary_op(operator.sub):
                    return number_error()
            case Op.MULTIPLY:
                if not binary_op(operator.mul):
                    return number_error()
            case Op.DIVIDE:
                if not binary_op(operator.truediv):
                    return number_error()

            case Op.NOT:
                push(is_falsey(pop()))

            case Op.NEGATE:
                value = peek(0)
                if not is_number(value):
                    return add_error()
                assert value == pop()
                push(-value)

            case Op.POP:
                pop()

            case Op.PRINT:
                print_value(pop())
                printf("\n")

            case Op.ASSERT:
                value = pop()
                if is_falsey(value):
                    runtime_error("Assertion error")
                    return InterpretAssertionError()

            case Op.DEFINE_GLOBAL:
                name = read_string(frame)
                vm.globals[name] = peek(0)
                pop()
            case Op.GET_GLOBAL:
                name = read_string(frame)
                if name not in vm.globals:
                    runtime_error("Undefined variable '{0:s}'.", name.value)
                    return InterpretError()
                push(vm.globals[name])
            case Op.SET_GLOBAL:
                name = read_string(frame)
                if name not in vm.globals:
                    runtime_error("Undefined variable '{0:s}'.", name.value)
                    return InterpretError()
                vm.globals[name] = peek(0)

            case Op.GET_LOCAL:
                slot = read_byte(frame)

                push(frame.slots[frame.stack_top + slot])
            case Op.SET_LOCAL:
                slot = read_byte(frame)

                frame.slots[frame.stack_top + slot] = peek(0)

            case Op.JUMP_IF_FALSE:
                offset = read_short(frame)
                if is_falsey(peek(0)):
                    frame.ip += offset
            case Op.JUMP:
                offset = read_short(frame)

                frame.ip += offset
            case Op.LOOP:
                offset = read_short(frame)

                frame.ip -= offset

            case Op.CALL:
                arg_count = read_byte(frame)

                if not call_value(peek(arg_count), arg_count):
                    return InterpretError()
                frame = vm.frames[-1]

            case Op.CLOSURE:
                function = as_function(read_constant(frame))
                closure = new_closure(function)
                push(closure)

                for i in range(closure.upvalue_count):
                    is_local = read_byte(frame)
                    index = read_byte(frame)
                    if is_local:
                        closure.upvalues[i] = capture_upvalue(
                            frame.slots[frame.stack_top + index]
                        )

                    else:
                        closure.upvalues[i] = frame.closure.upvalues[index]

            case Op.GET_UPVALUE:
                slot = read_byte(frame)

                push(frame.closure.upvalues[slot].location)

            case Op.SET_UPVALUE:
                slot = read_byte(frame)

                frame.closure.upvalues[slot].location = peek(0)

            case Op.CLOSE_UPVALUE:
                close_upvalues(vm.stack_top - 1)
                pop()

            case Op.CLASS:
                push(new_class(read_string(frame)))

            case Op.GET_PROPERTY:
                instance = peek(0)

                if not is_instance(instance):
                    runtime_error("Only instances have properties")
                    return InterpretError()

                name = read_string(frame)
                if name in instance.fields:
                    value = instance.fields[name]
                    pop()
                    push(value)
                else:
                    if not bind_method(instance.klass, name):
                        return InterpretError()

            case Op.SET_PROPERTY:
                instance = peek(1)
                if not is_instance(instance):
                    runtime_error("Only instances have properties")
                    return InterpretError()

                instance.fields[read_string(frame)] = peek(0)

                value = pop()
                pop()
                push(value)

            case Op.METHOD:
                define_method(read_string(frame))

            case Op.INVOKE:
                method = read_string(frame)
                arg_count = read_byte(frame)
                if not invoke(method, arg_count):
                    return InterpretError()
                frame = vm.frames[-1]

            case Op.INHERIT:
                superclass = as_class(peek(1))
                subclass = as_class(peek(0))
                superclass.methods.update(subclass.methods)
                pop()

            case Op.GET_SUPER:
                name = read_string(frame)
                superclass = as_class(pop())

                if not bind_method(superclass, name):
                    return InterpretError()

            case Op.SUPER_INVOKE:
                method = read_string(frame)
                arg_count = read_byte(frame)
                superclass = as_class(pop())
                if not invoke_from_class(superclass, method, arg_count):
                    return InterpretError()
                frame = vm.frames[-1]

            case Op.RETURN:
                result = pop()
                close_upvalues(frame.stack_top)
                vm.frames.pop()
                if len(vm.frames) == 0:
                    pop()
                    assert vm.stack_top == 0, vm.stack_top

                    return InterpretOk()

                vm.stack_top = frame.stack_top
                push(result)
                frame = vm.frames[-1]
            case _:
                raise Exception("Wrong instruction", Op(instruction))


def invoke_from_class(klass: ObjClass, name: ObjString, arg_count: int) -> bool:
    if name not in klass.methods:
        runtime_error("Undefined property '{0:s}'", name.value)
        return False
    method = klass.methods[name]
    return call(as_closure(method), arg_count)


def invoke(name: ObjString, arg_count: int) -> bool:
    receiver = peek(arg_count)

    if not is_instance(receiver):
        runtime_error("Only instances have methods.")
        return False

    instance = as_instance(receiver)

    if name in instance.fields:
        value = instance.fields[name]
        vm.stack[vm.stack_top - arg_count - 1] = value
        return call_value(value, arg_count)

    return invoke_from_class(instance.klass, name, arg_count)


def bind_method(klass: ObjClass, name: ObjString):
    if name not in klass.methods:
        runtime_error("Undefined property '{0:s}'", name.value)
        return False

    method = klass.methods[name]
    bound = new_bound_method(peek(0), as_closure(method))
    pop()
    push(bound)
    return True


def define_method(name: ObjString):
    method = peek(0)
    klass = as_class(peek(1))
    klass.methods[name] = method
    pop()


def close_upvalues(last: Value):
    while vm.open_upvalues is not None and id(vm.open_upvalues.location) >= id(last):
        upvalue = vm.open_upvalues
        upvalue.closed = upvalue.location
        upvalue.location = upvalue.closed
        vm.open_upvalues = upvalue.next


def capture_upvalue(local: Value) -> ObjUpvalue:
    prev_upvalue = None
    upvalue = vm.open_upvalues

    while upvalue is not None and id(upvalue.location) > id(local):
        prev_upvalue = upvalue
        upvalue = upvalue.next

    if upvalue is not None and upvalue.location == local:
        return upvalue

    created_upvalue = new_upvalue(local)

    created_upvalue.next = upvalue

    if prev_upvalue is None:
        vm.open_upvalues = created_upvalue
    else:
        prev_upvalue.next = created_upvalue

    return created_upvalue


def define_native(name: str, function: NativeFn):
    s = ObjString(name)
    n = new_native(function)

    push(s)
    push(n)

    vm.globals[s] = n
    pop()
    pop()


def clock_native(arg_count: int, args: Value) -> Value:
    return time.time()


def call_value(callee: Value, arg_count: int):
    if is_native(callee):
        stack_top = vm.stack_top - arg_count
        assert 0 <= stack_top <= vm.stack_top
        result = callee.function(arg_count, stack_top)
        vm.stack_top -= arg_count + 1
        push(result)
        return True
    elif is_bound_method(callee):
        bound = as_bound_method(callee)
        vm.stack[vm.stack_top - arg_count - 1] = bound.receiver
        return call(bound.method, arg_count)
    elif is_class(callee):
        klass = as_class(callee)
        vm.stack[vm.stack_top - arg_count - 1] = new_instance(klass)
        if vm.init_string in klass.methods:
            initializer = klass.methods[vm.init_string]
            return call(as_closure(initializer), arg_count)
        elif arg_count != 0:
            runtime_error("Expected 0 arguments but got {0:d}", arg_count)
            return False
        return True
    elif is_closure(callee):
        return call(callee, arg_count)

    runtime_error("Can only call functions and classes")
    return False


def call(closure: ObjClosure, arg_count: int):
    function = closure.function

    if arg_count != function.arity:
        runtime_error(
            "Expected {0:d} arguments but got {1:d}", function.arity, arg_count
        )
        return False

    stack_top = vm.stack_top - arg_count - 1
    assert 0 <= stack_top <= vm.stack_top
    frame = CallFrame(
        closure=closure,
        ip=0,
        stack_top=stack_top,
        slots=vm.stack,
    )

    vm.frames.append(frame)
    return True


def concatenate() -> bool:
    b, a = peek(0), peek(1)
    if not is_string(b) or not is_string(a):
        return False
    assert (b, a) == (pop(), pop())
    push(ObjString(a.value + b.value))
    return True


def is_falsey(value: Value) -> bool:
    if is_nil(value):
        return True
    if is_bool(value):
        return not value
    return False


def number_error() -> InterpretResult:
    runtime_error("Operands must be two numbers")
    return InterpretError()


def add_error() -> InterpretResult:
    runtime_error("Operands must be two numbers or two strings")
    return InterpretError()


def runtime_error(format: str, *args: Any):
    printf(format, *args)

    frame = vm.frames[-1]
    instruction = frame.ip
    line = frame.closure.function.chunk.lines[instruction]
    printf_err("[line {0:d}] in script\n", line)
    printf_err("{0:s}\n", frame.closure.function.name.value)
    reset_stack()


def reset_stack():
    vm.stack_top = 0


def peek(distance: int) -> Value:
    i = vm.stack_top - distance - 1
    return vm.stack[i]


def binary_op(fn: Callable[[Number, Number], Number]) -> bool:
    b, a = peek(0), peek(1)
    if not is_number(b) or not is_number(a):
        return False
    assert (b, a) == (pop(), pop())
    push(fn(a, b))
    return True


def push(value: Value):
    assert 0 <= vm.stack_top <= len(vm.stack)
    vm.stack[vm.stack_top] = value
    vm.stack_top += 1


def pop() -> Value:
    vm.stack_top -= 1
    assert 0 <= vm.stack_top <= len(vm.stack)
    return vm.stack[vm.stack_top]


def read_byte(frame: CallFrame) -> int:
    frame.ip += 1
    return frame.closure.function.chunk.code[frame.ip - 1]


def read_short(frame: CallFrame) -> int:
    frame.ip += 2
    return (
        frame.closure.function.chunk.code[frame.ip - 2] << 8
    ) | frame.closure.function.chunk.code[frame.ip - 1]


def read_constant(frame: CallFrame) -> Value:
    return frame.closure.function.chunk.constants[read_byte(frame)]


def read_string(frame: CallFrame) -> ObjString:
    return as_string(read_constant(frame))
