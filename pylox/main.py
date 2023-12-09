import sys

from .shared import printf

from .vm import CompileError, InterpretError, init_vm, interpret


def repl():
    while True:
        inpt = input()
        interpret(inpt)


def run_file(filepath: str):
    with open(filepath) as fp:
        result = interpret(fp.read())

    if isinstance(result, CompileError):
        sys.exit(65)
    if isinstance(result, InterpretError):
        sys.exit(70)


def main():
    init_vm()

    if len(sys.argv) == 1:
        repl()
    if len(sys.argv) == 2:
        run_file(sys.argv[1])
    else:
        printf("Usage: plox [path]\n")
        sys.exit(64)
