from .shared import printf
from .value import Chunk, as_function, print_value
from .value import Op


def disassemble_chunk(chunk: Chunk, name: str):
    printf("== {0:s} ==\n", name)

    offset = 0
    while offset < len(chunk.code):
        offset = disassemble_instruction(chunk, offset)


def disassemble_instruction(chunk: Chunk, offset: int) -> int:
    printf("{0:04d} ", offset)
    if offset > 0 and chunk.lines[offset] == chunk.lines[offset - 1]:
        printf("   | ")
    else:
        printf("{0:4d} ", chunk.lines[offset])

    instruction = chunk.code[offset]
    match instruction:
        case Op.RETURN:
            return simple_instruction("RETURN", offset)

        case Op.CONSTANT:
            return constant_instruction("CONSTANT", chunk, offset)

        case Op.DEFINE_GLOBAL:
            return constant_instruction("DEFINE_GLOBAL", chunk, offset)
        case Op.GET_GLOBAL:
            return constant_instruction("GET_GLOBAL", chunk, offset)
        case Op.SET_GLOBAL:
            return constant_instruction("SET_GLOBAL", chunk, offset)
        case Op.CLASS:
            return constant_instruction("CLASS", chunk, offset)

        case Op.GET_PROPERTY:
            return constant_instruction("GET_PROPERTY", chunk, offset)
        case Op.SET_PROPERTY:
            return constant_instruction("SET_PROPERTY", chunk, offset)

        case Op.METHOD:
            return constant_instruction("METHOD", chunk, offset)

        case Op.INVOKE:
            return invoke_instruction("INVOKE", chunk, offset)

        case Op.GET_LOCAL:
            return byte_instruction("GET_LOCAL", chunk, offset)
        case Op.SET_LOCAL:
            return byte_instruction("SET_LOCAL", chunk, offset)

        case Op.CALL:
            return byte_instruction("CALL", chunk, offset)

        case Op.NIL:
            return simple_instruction("NIL", offset)
        case Op.TRUE:
            return simple_instruction("TRUE", offset)
        case Op.FALSE:
            return simple_instruction("FALSE", offset)

        case Op.EQUAL:
            return simple_instruction("EQUAL", offset)
        case Op.GREATER:
            return simple_instruction("GREATER", offset)
        case Op.LESS:
            return simple_instruction("LESS", offset)

        case Op.ADD:
            return simple_instruction("ADD", offset)
        case Op.SUBTRACT:
            return simple_instruction("SUBTRACT", offset)
        case Op.MULTIPLY:
            return simple_instruction("MULTIPLY", offset)
        case Op.DIVIDE:
            return simple_instruction("DIVIDE", offset)

        case Op.NOT:
            return simple_instruction("NOT", offset)

        case Op.NEGATE:
            return simple_instruction("NEGATE", offset)

        case Op.PRINT:
            return simple_instruction("PRINT", offset)
        case Op.ASSERT:
            return simple_instruction("ASSERT", offset)

        case Op.JUMP:
            return jump_instruction("JUMP", 1, chunk, offset)
        case Op.JUMP_IF_FALSE:
            return jump_instruction("JUMP_IF_FALSE", 1, chunk, offset)
        case Op.LOOP:
            return jump_instruction("LOOP", -11, chunk, offset)

        case Op.POP:
            return simple_instruction("POP", offset)

        case Op.GET_UPVALUE:
            return byte_instruction("GET_UPVALUE", chunk, offset)
        case Op.SET_UPVALUE:
            return byte_instruction("SET_UPVALUE", chunk, offset)

        case Op.INHERIT:
            return simple_instruction("INHERIT", offset)

        case Op.GET_SUPER:
            return constant_instruction("GET_SUPER", chunk, offset)

        case Op.SUPER_INVOKE:
            return invoke_instruction("SUPER_INVOKE", chunk, offset)

        case Op.CLOSURE:
            offset += 1
            constant = chunk.code[offset]
            offset += 1
            printf("{0:<16s} {1:4d} ", "CLOSURE", constant)
            print_value(chunk.constants[constant])
            printf("\n")

            function = as_function(chunk.constants[constant])
            for _ in range(function.upvalue_count):
                is_local = chunk.code[offset]
                offset += 1
                index = chunk.code[offset]
                offset += 1
                printf(
                    "{0:04d}      |                     {1:s} {2:d}\n",
                    offset - 2,
                    "local" if is_local else "upvalue",
                    index,
                )
            return offset

        case Op.CLOSE_UPVALUE:
            return simple_instruction("CLOSE_UPVALUE", offset)

        case _:
            printf("Unknown opcode {0:d}\n", instruction)
            return offset + 1


def invoke_instruction(name: str, chunk: Chunk, offset: int) -> int:
    constant = chunk.code[offset + 1]
    arg_count = chunk.code[offset + 2]
    printf("{0:<16s} ({1:d} args) {2:4d} '", name, arg_count, constant)
    print_value(chunk.constants[constant])
    printf("'\n")
    return offset + 3


def jump_instruction(name: str, sign: int, chunk: Chunk, offset: int) -> int:
    jump = (chunk.code[offset + 1] << 8) | chunk.code[offset + 2]
    printf("{0:<16s} {1:4d} -> {2:d}\n", name, offset, offset + 3 + sign * jump)
    return offset + 3


def simple_instruction(name: str, offset: int) -> int:
    printf("{0:s}\n", name)
    return offset + 1


def constant_instruction(name: str, chunk: Chunk, offset: int) -> int:
    constant = chunk.code[offset + 1]
    printf("{0:<16s} {1:4d} '", name, constant)
    print_value(chunk.constants[constant])
    printf("'\n")
    return offset + 2


def byte_instruction(name: str, chunk: Chunk, offset: int) -> int:
    slot = chunk.code[offset + 1]
    printf("{0:<16s} {1:4d} '", name, slot)
    printf("'\n")
    return offset + 2
