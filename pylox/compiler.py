from dataclasses import dataclass
import enum
from typing import Callable

from .value import (
    Chunk,
    ObjFunction,
    ObjString,
    Value,
    add_constant,
    byte,
    new_function,
    write_chunk,
)
from .debug import disassemble_chunk
from .shared import printf_err
from .value import Op
from .scanner import Token, TokenType, init_scanner, scan_token


@dataclass
class Local:
    name: Token
    depth: int
    is_captured: bool


class FunctionType(enum.Enum):
    FUNCTION = enum.auto()
    SCRIPT = enum.auto()
    METHOD = enum.auto()
    INITIALIZER = enum.auto()


@dataclass
class Upvalue:
    index: int = 0
    is_local: bool = False


@dataclass
class Compiler:
    function: ObjFunction
    type: FunctionType
    locals: list[Local]
    upvalues: list[Upvalue]
    scope_depth: int
    enclosing: "Compiler|None"


current: Compiler | None = None


@dataclass
class ClassCompiler:
    enclosing: "ClassCompiler|None"
    has_superclass: bool


current_class: ClassCompiler | None = None


def init_compiler(typ: FunctionType) -> Compiler:
    compiler = Compiler(
        function=new_function(),
        type=typ,
        locals=[],
        upvalues=[Upvalue() for _ in range(_UINT8_MAX)],
        scope_depth=0,
        enclosing=None,
    )

    global current
    compiler.enclosing = current

    current = compiler

    if typ != FunctionType.SCRIPT:
        current.function.name = ObjString(parser.previous.lexeme)

    local = Local(name=Token(TokenType.EOF, "<script>", 0), depth=0, is_captured=False)
    current.locals.append(local)

    if typ != FunctionType.FUNCTION:
        local.name.lexeme = "this"
    else:
        local.name.lexeme = ""

    return compiler


@dataclass
class Parser:
    current: Token
    previous: Token
    had_error: bool
    panic_mode: bool


parser: Parser


def init_parser():
    global parser
    parser = Parser(
        current=Token(TokenType.EOF, "", 0),
        previous=Token(TokenType.EOF, "", 0),
        had_error=False,
        panic_mode=False,
    )


def compile_source(source: str) -> ObjFunction | None:
    init_scanner(source)
    init_parser()

    init_compiler(FunctionType.SCRIPT)

    advance()

    while not match(TokenType.EOF):
        declaration()

    function = end_compiler()

    if parser.had_error:
        return None
    return function


def declaration():
    if match(TokenType.CLASS):
        class_declaration()
    elif match(TokenType.FUN):
        fun_declaration()
    elif match(TokenType.VAR):
        var_declaration()
    else:
        statement()

    if parser.panic_mode:
        synchronize()


def synthetic_token(text: str) -> Token:
    return Token(type=TokenType.EOF, lexeme=text, line=0)


def class_declaration():
    consume(TokenType.IDENTIFIER, "Expect class name")
    class_name = parser.previous
    name_constant = identifier_constant(parser.previous)
    declare_variable()

    emit_bytes(Op.CLASS, name_constant)
    define_variable(name_constant)

    global current_class
    class_compiler = ClassCompiler(enclosing=current_class, has_superclass=False)
    current_class = class_compiler

    if match(TokenType.LESS):
        consume(TokenType.IDENTIFIER, "Expect superclass name")
        variable(False)
        if identifiers_equal(class_name, parser.previous):
            error("A class can't inherit from itself")

        begin_scope()
        add_local(synthetic_token("super"))
        define_variable(0)

        named_variable(class_name, False)
        emit_byte(Op.INHERIT)

        class_compiler.has_superclass = True

    named_variable(class_name, False)
    consume(TokenType.LEFT_BRACE, "Expect '{' before class body")
    while not check(TokenType.RIGHT_BRACE) and not check(TokenType.EOF):
        method()
    consume(TokenType.RIGHT_BRACE, "Expect '}' after class body")
    emit_byte(Op.POP)

    if class_compiler.has_superclass:
        end_scope()

    current_class = current_class.enclosing


def method():
    consume(TokenType.IDENTIFIER, "Expect method name")
    constant = identifier_constant(parser.previous)

    type = FunctionType.METHOD

    if len(parser.previous.lexeme) == 4 and parser.previous.lexeme.startswith("init"):
        type = FunctionType.INITIALIZER

    function(type)
    emit_bytes(Op.METHOD, constant)


def fun_declaration():
    var = parse_variable("Expect function name")
    mark_initialized()
    function(FunctionType.FUNCTION)
    define_variable(var)


def function(typ: FunctionType):
    compiler = init_compiler(typ)

    begin_scope()

    consume(TokenType.LEFT_PAREN, "Expect '(' after function name")

    if not check(TokenType.RIGHT_PAREN):
        while True:
            current.function.arity += 1
            if current.function.arity > 255:
                error_at_current("Can't have more than 255 parameters")
            constant = parse_variable("Expect parameter name")
            define_variable(constant)
            if not match(TokenType.COMMA):
                break

    consume(TokenType.RIGHT_PAREN, "Expect ')' after parameters")
    consume(TokenType.LEFT_BRACE, "Expect '{' before function body")
    block()

    function = end_compiler()

    emit_bytes(Op.CLOSURE, make_constant(function))

    for i in range(function.upvalue_count):
        emit_byte(int(compiler.upvalues[i].is_local))
        emit_byte(compiler.upvalues[i].index)


def var_declaration():
    var = parse_variable("Expect variable name")

    if match(TokenType.EQUAL):
        expression()
    else:
        emit_byte(Op.NIL)

    consume(TokenType.SEMICOLON, "Expect ';' after variable declaration")

    define_variable(var)


def parse_variable(error_message: str) -> int:
    consume(TokenType.IDENTIFIER, error_message)

    declare_variable()
    if current.scope_depth > 0:
        return 0

    return identifier_constant(parser.previous)


def declare_variable():
    if current.scope_depth == 0:
        return 0

    name = parser.previous
    for local in reversed(current.locals):
        if local.depth != -1 and local.depth < current.scope_depth:
            break
        if identifiers_equal(name, local.name):
            error("Already a variable with this name in this scope")

    add_local(name)


def add_local(name: Token):
    if len(current.locals) == _UINT8_MAX:
        error("Too many local variables in function")
        return

    current.locals.append(Local(name=name, depth=-1, is_captured=False))


def identifier_constant(token: Token) -> int:
    return make_constant(ObjString(token.lexeme))


def define_variable(global_var: int):
    if current.scope_depth > 0:
        mark_initialized()
        return
    emit_bytes(Op.DEFINE_GLOBAL, global_var)


def mark_initialized():
    if current.scope_depth == 0:
        return
    current.locals[-1].depth = current.scope_depth


def synchronize():
    parser.panic_mode = False

    while parser.current.type != TokenType.EOF:
        if parser.previous.type == TokenType.SEMICOLON:
            return
        match parser.current.type:
            case TokenType.CLASS:
                return
            case TokenType.FUN:
                return
            case TokenType.VAR:
                return
            case TokenType.FOR:
                return
            case TokenType.IF:
                return
            case TokenType.WHILE:
                return
            case TokenType.PRINT:
                return
            case TokenType.RETURN:
                return
            case _:
                pass

        advance()


def statement():
    if match(TokenType.PRINT):
        print_statement()
    elif match(TokenType.FOR):
        for_statement()
    elif match(TokenType.IF):
        if_statement()
    elif match(TokenType.RETURN):
        return_statement()
    elif match(TokenType.WHILE):
        while_statement()
    elif match(TokenType.ASSERT):
        assert_statement()
    elif match(TokenType.LEFT_BRACE):
        begin_scope()
        block()
        end_scope()
    else:
        expression_statement()


def return_statement():
    if current.type == FunctionType.SCRIPT:
        error("Can't return from top-level code.")

    if match(TokenType.SEMICOLON):
        emit_return()
    else:
        if current.type == FunctionType.INITIALIZER:
            error("Can't return a value from an initializer.")

        expression()
        consume(TokenType.SEMICOLON, "Expect ';' after return value")
        emit_byte(Op.RETURN)


def begin_scope():
    current.scope_depth += 1


def end_scope():
    current.scope_depth -= 1

    while len(current.locals) > 0 and current.locals[-1].depth > current.scope_depth:
        if current.locals[-1].is_captured:
            emit_byte(Op.CLOSE_UPVALUE)
        else:
            emit_byte(Op.POP)
        current.locals.pop()


def block():
    while not check(TokenType.RIGHT_BRACE) and not check(TokenType.EOF):
        declaration()

    consume(TokenType.RIGHT_BRACE, "Expect '}' after block")


def print_statement():
    expression()
    consume(TokenType.SEMICOLON, "Expect ';' after value")
    emit_byte(Op.PRINT)


def assert_statement():
    expression()
    consume(TokenType.SEMICOLON, "Expect ';' after value")
    emit_byte(Op.ASSERT)


def if_statement():
    consume(TokenType.LEFT_PAREN, "Expect '(' after 'if'")
    expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition")

    then_jump = emit_jump(Op.JUMP_IF_FALSE)
    emit_byte(Op.POP)
    statement()

    else_jump = emit_jump(Op.JUMP)

    patch_jump(then_jump)
    emit_byte(Op.POP)

    if match(TokenType.ELSE):
        statement()

    patch_jump(else_jump)


def for_statement():
    begin_scope()

    consume(TokenType.LEFT_PAREN, "Expect '(' after 'for'.")

    if match(TokenType.SEMICOLON):
        pass
    elif match(TokenType.VAR):
        var_declaration()
    else:
        expression_statement()

    loop_start = len(current_chunk().code)
    exit_jump = -1

    if not match(TokenType.SEMICOLON):
        expression()
        consume(TokenType.SEMICOLON, "Expect ';' after loop condition")

        exit_jump = emit_jump(Op.JUMP_IF_FALSE)
        emit_byte(Op.POP)

    if not match(TokenType.RIGHT_PAREN):
        body_jump = emit_jump(Op.JUMP)
        increment_start = len(current_chunk().code)
        expression()
        emit_byte(Op.POP)
        consume(TokenType.RIGHT_PAREN, "Expect ')' after for clauses.")

        emit_loop(loop_start)
        loop_start = increment_start
        patch_jump(body_jump)

    statement()
    emit_loop(loop_start)

    if exit_jump != -1:
        patch_jump(exit_jump)
        emit_byte(Op.POP)

    end_scope()


def while_statement():
    loop_start = len(current_chunk().code)

    consume(TokenType.LEFT_PAREN, "Expect '(' after 'while'")
    expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after condition")

    exit_jump = emit_jump(Op.JUMP_IF_FALSE)
    emit_byte(Op.POP)
    statement()

    emit_loop(loop_start)

    patch_jump(exit_jump)
    emit_byte(Op.POP)


_UINT8_MAX = 256
_UINT16_MAX = 2**16 - 1


def emit_loop(loop_start: int):
    emit_byte(Op.LOOP)

    offset = len(current_chunk().code) - loop_start + 2
    if offset > _UINT16_MAX:
        error("Loop body too large")

    emit_byte((offset >> 8) & 0xFF)
    emit_byte(offset & 0xFF)


def emit_jump(instruction: Op) -> int:
    emit_byte(instruction)
    emit_byte(0xFF)
    emit_byte(0xFF)
    return len(current_chunk().code) - 2


def patch_jump(offset: int):
    jump = len(current_chunk().code) - offset - 2
    if jump > _UINT16_MAX:
        error("Too much code to jump over")

    assert current_chunk().code[offset] == 0xFF
    assert current_chunk().code[offset + 1] == 0xFF

    current_chunk().code[offset] = (jump >> 8) & 0xFF
    current_chunk().code[offset + 1] = jump & 0xFF


def expression_statement():
    expression()
    consume(TokenType.SEMICOLON, "Expect ';' after expression")
    emit_byte(Op.POP)


def expression():
    parse_precedence(Precedence.ASSIGNMENT)


class Precedence(enum.IntEnum):
    NONE = enum.auto()
    ASSIGNMENT = enum.auto()
    OR = enum.auto()
    AND = enum.auto()
    EQUALITY = enum.auto()
    COMPARISON = enum.auto()
    TERM = enum.auto()
    FACTOR = enum.auto()
    UNARY = enum.auto()
    CALL = enum.auto()
    PRIMARY = enum.auto()


def parse_precedence(precedence: Precedence):
    advance()
    prefix_rule = get_rule(parser.previous.type).prefix

    if prefix_rule is None:
        error("Expect expression")
        return

    can_assign = precedence <= Precedence.ASSIGNMENT
    prefix_rule(can_assign)

    while precedence <= get_rule(parser.current.type).precedence:
        advance()
        infix_rule = get_rule(parser.previous.type).infix
        if infix_rule is None:
            raise Exception("Infix is empty", infix_rule)
        infix_rule(can_assign)

    if can_assign and match(TokenType.EQUAL):
        error("Invalid assignment target")


def grouping(can_assign: bool):
    expression()
    consume(TokenType.RIGHT_PAREN, "Expect ')' after expression")


@dataclass
class ParseRule:
    prefix: Callable[[bool], None] | None
    infix: Callable[[bool], None] | None
    precedence: Precedence


def get_rule(typ: TokenType) -> ParseRule:
    return _RULES[typ]


def binary(can_assign: bool):
    typ = parser.previous.type
    rule = get_rule(typ)
    parse_precedence(Precedence(rule.precedence + 1))

    match typ:
        case TokenType.BANG_EQUAL:
            emit_bytes(Op.EQUAL, Op.NOT)
        case TokenType.EQUAL_EQUAL:
            emit_byte(Op.EQUAL)
        case TokenType.GREATER:
            emit_byte(Op.GREATER)
        case TokenType.GREATER_EQUAL:
            emit_bytes(Op.LESS, Op.NOT)
        case TokenType.LESS:
            emit_byte(Op.LESS)
        case TokenType.LESS_EQUAL:
            emit_bytes(Op.GREATER, Op.NOT)

        case TokenType.PLUS:
            emit_byte(Op.ADD)
        case TokenType.MINUS:
            emit_byte(Op.SUBTRACT)
        case TokenType.STAR:
            emit_byte(Op.MULTIPLY)
        case TokenType.SLASH:
            emit_byte(Op.DIVIDE)
        case _:
            raise Exception("Wrong binary token type", typ)


def call(can_assign: bool):
    arg_count = argument_list()
    emit_bytes(Op.CALL, arg_count)


def dot(can_assign: bool):
    consume(TokenType.IDENTIFIER, "Expect property name after '.'")
    name = identifier_constant(parser.previous)

    if can_assign and match(TokenType.EQUAL):
        expression()
        emit_bytes(Op.SET_PROPERTY, name)
    elif match(TokenType.LEFT_PAREN):
        arg_count = argument_list()
        emit_bytes(Op.INVOKE, name)
        emit_byte(arg_count)
    else:
        emit_bytes(Op.GET_PROPERTY, name)


def argument_list() -> int:
    arg_count = 0
    if not check(TokenType.RIGHT_PAREN):
        while True:
            expression()
            if arg_count == 255:
                error("Can't have more than 255 arguments")
            arg_count += 1
            if not match(TokenType.COMMA):
                break

    consume(TokenType.RIGHT_PAREN, "Expect ')' after arguments")
    return arg_count


def unary(can_assign: bool):
    typ = parser.previous.type

    parse_precedence(Precedence.UNARY)

    match typ:
        case TokenType.MINUS:
            emit_byte(Op.NEGATE)
        case TokenType.BANG:
            emit_byte(Op.NOT)
        case _:
            raise Exception("Wrong unary token type", typ)


def literal(can_assign: bool):
    typ = parser.previous.type
    match typ:
        case TokenType.FALSE:
            emit_byte(Op.FALSE)
        case TokenType.NIL:
            emit_byte(Op.NIL)
        case TokenType.TRUE:
            emit_byte(Op.TRUE)
        case _:
            raise Exception("Wrong literal token type", typ)


def variable(can_assign: bool):
    named_variable(parser.previous, can_assign)


def named_variable(name: Token, can_assign: bool):
    arg = resolve_local(current, name)
    if arg != -1:
        get_opt = Op.GET_LOCAL
        set_opt = Op.SET_LOCAL
    else:
        arg = resolve_upvalue(current, name)
        if arg != -1:
            get_opt = Op.GET_UPVALUE
            set_opt = Op.SET_UPVALUE
        else:
            arg = identifier_constant(name)
            get_opt = Op.GET_GLOBAL
            set_opt = Op.SET_GLOBAL

    if can_assign and match(TokenType.EQUAL):
        expression()
        emit_bytes(set_opt, arg)
    else:
        emit_bytes(get_opt, arg)


def resolve_upvalue(compiler: Compiler, name: Token) -> int:
    if compiler.enclosing is None:
        return -1

    local = resolve_local(compiler.enclosing, name)
    if local != -1:
        compiler.enclosing.locals[local].is_captured = True
        return add_upvalue(compiler, local, True)

    upvalue = resolve_upvalue(compiler.enclosing, name)
    if upvalue != -1:
        return add_upvalue(compiler, upvalue, False)

    return -1


def add_upvalue(compiler: Compiler, index: int, is_local: bool) -> int:
    upvalue_count = compiler.function.upvalue_count

    for i in range(upvalue_count):
        upvalue = compiler.upvalues[i]
        if upvalue.index == index and upvalue.is_local == is_local:
            return i

    if upvalue_count == _UINT8_MAX:
        error("Too many closure variables in function")
        return 0

    compiler.upvalues[upvalue_count].is_local = is_local
    compiler.upvalues[upvalue_count].index = index
    compiler.function.upvalue_count += 1
    return compiler.function.upvalue_count - 1


def resolve_local(compiler: Compiler, name: Token) -> int:
    for i, local in reversed(list(enumerate(compiler.locals))):
        if identifiers_equal(local.name, name):
            if local.depth == -1:
                error("Can't read local variable in its own initializer")
            return i

    return -1


def identifiers_equal(a: Token, b: Token) -> bool:
    return a.lexeme == b.lexeme


def string(can_assign: bool):
    emit_constant(ObjString(parser.previous.lexeme[1:-1]))


def number(can_assign: bool):
    value = float(parser.previous.lexeme)
    emit_constant(value)


def and_(can_assign: bool):
    end_jump = emit_jump(Op.JUMP_IF_FALSE)

    emit_byte(Op.POP)
    parse_precedence(Precedence.AND)

    patch_jump(end_jump)


def or_(can_assign: bool):
    else_jump = emit_jump(Op.JUMP_IF_FALSE)
    end_jump = emit_jump(Op.JUMP)

    patch_jump(else_jump)
    emit_byte(Op.POP)

    parse_precedence(Precedence.OR)
    patch_jump(end_jump)


def this_(value: Value):
    if current_class is None:
        error("Can't use 'this' outside of a class")
        return

    variable(False)


def super_(can_assign: bool):
    if current_class is None:
        error("Can't use 'super' outside of a class")
    elif not current_class.has_superclass:
        error("Can't use 'super' in a class with no superclass")

    consume(TokenType.DOT, "Expect '.' after 'super'")
    consume(TokenType.IDENTIFIER, "Expect superclass method name")
    name = identifier_constant(parser.previous)

    named_variable(synthetic_token("this"), False)

    if match(TokenType.LEFT_PAREN):
        arg_count = argument_list()
        named_variable(synthetic_token("super"), False)
        emit_bytes(Op.SUPER_INVOKE, name)
        emit_byte(arg_count)
    else:
        named_variable(synthetic_token("super"), False)
        emit_bytes(Op.GET_SUPER, name)


_RULES: dict[TokenType, ParseRule] = {
    TokenType.LEFT_PAREN: ParseRule(grouping, call, Precedence.CALL),
    TokenType.RIGHT_PAREN: ParseRule(None, None, Precedence.NONE),
    TokenType.LEFT_BRACE: ParseRule(None, None, Precedence.NONE),
    TokenType.RIGHT_BRACE: ParseRule(None, None, Precedence.NONE),
    TokenType.COMMA: ParseRule(None, None, Precedence.NONE),
    TokenType.DOT: ParseRule(None, dot, Precedence.CALL),
    TokenType.MINUS: ParseRule(unary, binary, Precedence.TERM),
    TokenType.PLUS: ParseRule(None, binary, Precedence.TERM),
    TokenType.SEMICOLON: ParseRule(None, None, Precedence.NONE),
    TokenType.SLASH: ParseRule(None, binary, Precedence.FACTOR),
    TokenType.STAR: ParseRule(None, binary, Precedence.FACTOR),
    TokenType.BANG: ParseRule(unary, None, Precedence.NONE),
    TokenType.BANG_EQUAL: ParseRule(None, binary, Precedence.EQUALITY),
    TokenType.EQUAL: ParseRule(None, None, Precedence.NONE),
    TokenType.EQUAL_EQUAL: ParseRule(None, binary, Precedence.EQUALITY),
    TokenType.GREATER: ParseRule(None, binary, Precedence.COMPARISON),
    TokenType.GREATER_EQUAL: ParseRule(None, binary, Precedence.COMPARISON),
    TokenType.LESS: ParseRule(None, binary, Precedence.COMPARISON),
    TokenType.LESS_EQUAL: ParseRule(None, binary, Precedence.COMPARISON),
    TokenType.IDENTIFIER: ParseRule(variable, None, Precedence.NONE),
    TokenType.STRING: ParseRule(string, None, Precedence.NONE),
    TokenType.NUMBER: ParseRule(number, None, Precedence.NONE),
    TokenType.AND: ParseRule(None, and_, Precedence.NONE),
    TokenType.CLASS: ParseRule(None, None, Precedence.NONE),
    TokenType.ELSE: ParseRule(None, None, Precedence.NONE),
    TokenType.FALSE: ParseRule(literal, None, Precedence.NONE),
    TokenType.FOR: ParseRule(None, None, Precedence.NONE),
    TokenType.FUN: ParseRule(None, None, Precedence.NONE),
    TokenType.IF: ParseRule(None, None, Precedence.NONE),
    TokenType.NIL: ParseRule(literal, None, Precedence.NONE),
    TokenType.OR: ParseRule(None, or_, Precedence.NONE),
    TokenType.PRINT: ParseRule(None, None, Precedence.NONE),
    TokenType.ASSERT: ParseRule(None, None, Precedence.NONE),
    TokenType.RETURN: ParseRule(None, None, Precedence.NONE),
    TokenType.SUPER: ParseRule(super_, None, Precedence.NONE),
    TokenType.THIS: ParseRule(this_, None, Precedence.NONE),
    TokenType.TRUE: ParseRule(literal, None, Precedence.NONE),
    TokenType.VAR: ParseRule(None, None, Precedence.NONE),
    TokenType.WHILE: ParseRule(None, None, Precedence.NONE),
    TokenType.ERROR: ParseRule(None, None, Precedence.NONE),
    TokenType.EOF: ParseRule(None, None, Precedence.NONE),
}


def emit_constant(value: Value):
    emit_bytes(Op.CONSTANT, make_constant(value))


def make_constant(value: Value):
    constant = add_constant(current_chunk(), value)
    if constant > 255:
        error("Too many constants in one chunk")
        return 0

    return constant


def end_compiler() -> ObjFunction:
    emit_return()

    global current

    function = current.function

    if _debug_print_code:
        if not parser.had_error:
            disassemble_chunk(current_chunk(), function.name.value)

    current = current.enclosing
    return function


_debug_print_code = False


def set_debug_print_code(b: bool):
    global _debug_print_code
    _debug_print_code = b


def emit_return():
    if current.type == FunctionType.INITIALIZER:
        emit_bytes(Op.GET_LOCAL, 0)
    else:
        emit_byte(Op.NIL)

    emit_byte(Op.RETURN)


def consume(typ: TokenType, message: str):
    if parser.current.type == typ:
        advance()
        return

    error_at_current(message)


def emit_bytes(b1: byte, b2: byte):
    emit_byte(b1)
    emit_byte(b2)


def emit_byte(b: byte):
    write_chunk(current_chunk(), b, parser.previous.line)


def current_chunk() -> Chunk:
    return current.function.chunk


def match(typ: TokenType):
    if not check(typ):
        return False
    advance()
    return True


def check(typ: TokenType):
    return parser.current.type == typ


def advance():
    parser.previous = parser.current

    while True:
        parser.current = scan_token()
        if parser.current.type != TokenType.ERROR:
            break

        error_at_current(parser.current.lexeme)


def error_at_current(message: str):
    error_at(parser.current, message)


def error(message: str):
    error_at(parser.previous, message)


def error_at(token: Token, message: str):
    if parser.panic_mode:
        return
    parser.panic_mode = True

    printf_err("[line {0:d}] Error", token.line)
    if token.type == TokenType.EOF:
        printf_err("at end")
    elif token.type == TokenType.ERROR:
        pass
    else:
        printf_err(" at '{0:s}'\n", token.lexeme)

    printf_err(": {0:s}\n", message)
    parser.had_error = True
