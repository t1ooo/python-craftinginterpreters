from dataclasses import dataclass
import enum

_EOF = "\0"


@dataclass
class Scanner:
    source: str
    start: int
    current: int
    line: int

    def __post_init__(self):
        if not self.source.endswith(_EOF):
            raise Exception(f"Source should end with '{_EOF}'")


scanner: Scanner


def init_scanner(source: str):
    global scanner
    scanner = Scanner(source=source + _EOF, start=0, current=0, line=1)


class TokenType(enum.Enum):
    LEFT_PAREN = enum.auto()
    RIGHT_PAREN = enum.auto()
    LEFT_BRACE = enum.auto()
    RIGHT_BRACE = enum.auto()
    COMMA = enum.auto()
    DOT = enum.auto()
    MINUS = enum.auto()
    PLUS = enum.auto()
    SEMICOLON = enum.auto()
    SLASH = enum.auto()
    STAR = enum.auto()

    BANG = enum.auto()
    BANG_EQUAL = enum.auto()
    EQUAL = enum.auto()
    EQUAL_EQUAL = enum.auto()
    GREATER = enum.auto()
    GREATER_EQUAL = enum.auto()
    LESS = enum.auto()
    LESS_EQUAL = enum.auto()

    IDENTIFIER = enum.auto()
    STRING = enum.auto()
    NUMBER = enum.auto()

    AND = enum.auto()
    CLASS = enum.auto()
    ELSE = enum.auto()
    FALSE = enum.auto()
    FOR = enum.auto()
    FUN = enum.auto()
    IF = enum.auto()
    NIL = enum.auto()
    OR = enum.auto()
    PRINT = enum.auto()
    ASSERT = enum.auto()
    RETURN = enum.auto()
    SUPER = enum.auto()
    THIS = enum.auto()
    TRUE = enum.auto()
    VAR = enum.auto()
    WHILE = enum.auto()

    ERROR = enum.auto()
    EOF = enum.auto()


@dataclass
class Token:
    type: TokenType
    lexeme: str
    line: int


def scan_token() -> Token:
    skip_whitespace()

    scanner.start = scanner.current

    if is_at_end():
        return make_token(TokenType.EOF)

    c = advance()
    match c:
        case "(":
            return make_token(TokenType.LEFT_PAREN)
        case ")":
            return make_token(TokenType.RIGHT_PAREN)
        case "{":
            return make_token(TokenType.LEFT_BRACE)
        case "}":
            return make_token(TokenType.RIGHT_BRACE)
        case ";":
            return make_token(TokenType.SEMICOLON)
        case ",":
            return make_token(TokenType.COMMA)
        case ".":
            return make_token(TokenType.DOT)
        case "-":
            return make_token(TokenType.MINUS)
        case "+":
            return make_token(TokenType.PLUS)
        case "/":
            return make_token(TokenType.SLASH)
        case "*":
            return make_token(TokenType.STAR)

        case "!":
            return make_token(TokenType.BANG_EQUAL if match("=") else TokenType.BANG)
        case "=":
            return make_token(TokenType.EQUAL_EQUAL if match("=") else TokenType.EQUAL)
        case "<":
            return make_token(TokenType.LESS_EQUAL if match("=") else TokenType.LESS)
        case ">":
            return make_token(
                TokenType.GREATER_EQUAL if match("=") else TokenType.GREATER
            )

        case '"':
            return string()
        case _:
            if is_digit(c):
                return number()
            if is_alpha(c):
                return identifier()

            return error_token("Unexpected character")


def identifier_type() -> TokenType:
    lexeme = scanner.source[scanner.start : scanner.current]
    match lexeme:
        case "and":
            return TokenType.AND
        case "class":
            return TokenType.CLASS
        case "else":
            return TokenType.ELSE
        case "false":
            return TokenType.FALSE
        case "for":
            return TokenType.FOR
        case "fun":
            return TokenType.FUN
        case "if":
            return TokenType.IF
        case "nil":
            return TokenType.NIL
        case "or":
            return TokenType.OR
        case "print":
            return TokenType.PRINT
        case "assert":
            return TokenType.ASSERT
        case "return":
            return TokenType.RETURN
        case "super":
            return TokenType.SUPER
        case "this":
            return TokenType.THIS
        case "true":
            return TokenType.TRUE
        case "var":
            return TokenType.VAR
        case "while":
            return TokenType.WHILE
        case _:
            return TokenType.IDENTIFIER


def identifier() -> Token:
    while is_alpha(peek()) or is_digit(peek()):
        advance()

    return make_token(identifier_type())


def is_alpha(c: str) -> bool:
    return "a" <= c <= "z" or "A" <= c <= "Z" or c == "_"


def number() -> Token:
    while is_digit(peek()):
        advance()

    if peek() == "." and is_digit(peek_next()):
        advance()
        while is_digit(peek()):
            advance()

    return make_token(TokenType.NUMBER)


def is_digit(c: str) -> bool:
    return "0" <= c <= "9"


def string() -> Token:
    while peek() != '"' and not is_at_end():
        if peek() == "\n":
            scanner.line += 1
        advance()

    if is_at_end():
        return error_token("Unterminated string")

    advance()
    return make_token(TokenType.STRING)


def skip_whitespace():
    while True:
        c = peek()
        match c:
            case " ":
                advance()
            case "\r":
                advance()
            case "\t":
                advance()
            case "\n":
                scanner.line += 1
                advance()
            case "/":
                if peek_next() == "/":
                    while peek() != "\n" and not is_at_end():
                        advance()
                else:
                    return
            case _:
                return


def peek_next() -> str:
    if is_at_end():
        return _EOF
    return scanner.source[scanner.current + 1]


def peek() -> str:
    return scanner.source[scanner.current]


def match(expected: str) -> bool:
    if is_at_end():
        return False
    if scanner.source[scanner.current] != expected:
        return False
    scanner.current += 1
    return True


def advance() -> str:
    scanner.current += 1
    return scanner.source[scanner.current - 1]


def make_token(typ: TokenType) -> Token:
    return Token(
        type=typ,
        lexeme=scanner.source[scanner.start : scanner.current],
        line=scanner.line,
    )


def error_token(message: str) -> Token:
    return Token(
        type=TokenType.ERROR,
        lexeme=message,
        line=scanner.line,
    )


def is_at_end() -> bool:
    return scanner.source[scanner.current] == _EOF
