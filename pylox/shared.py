from sys import stderr
from typing import Any


def printf(format:str, *args:Any):
    print(format.format(*args), end="")


def printf_err(format:str, *args:Any):
    print(format.format(*args), end="", file=stderr)