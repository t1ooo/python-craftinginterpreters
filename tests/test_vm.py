from pylox.vm import (
    InterpretAssertionError,
    InterpretOk,
    init_vm,
    interpret,
)


def test_simple():
    init_vm()
    assert interpret("assert 1 == 1;") == InterpretOk()
    assert interpret("assert 1 == 2;") == InterpretAssertionError()

    assert interpret(f"assert 2+3 == {2+3};") == InterpretOk()
    assert interpret(f"assert 2-3 == {2-3};") == InterpretOk()
    assert interpret(f"assert 2*3 == {2*3};") == InterpretOk()
    assert interpret(f"assert 2/3 == {2/3};") == InterpretOk()

    assert interpret(f"assert (1+2)*3/4 == {(1+2)*3/4};") == InterpretOk()
    assert interpret(f"assert 1+2*3/4 == {1+2*3/4};") == InterpretOk()

    assert interpret("assert true;") == InterpretOk()
    assert interpret("assert !false;") == InterpretOk()
    assert interpret("assert !nil;") == InterpretOk()
    assert interpret("assert 1;") == InterpretOk()
    assert interpret("assert !!1;") == InterpretOk()

    assert interpret("assert !(1 == 2);") == InterpretOk()
    assert interpret("assert 2 == 2;") == InterpretOk()
    assert interpret("assert 1 != 2;") == InterpretOk()
    assert interpret("assert !(2 != 2);") == InterpretOk()

    assert interpret("assert !(1 > 2);") == InterpretOk()
    assert interpret("assert 1 < 2;") == InterpretOk()

    assert interpret('assert "a" == "a";') == InterpretOk()
    assert interpret('assert !("a" != "a");') == InterpretOk()
    assert interpret('assert !("a" == "b");') == InterpretOk()

    assert interpret('assert "a"+"b" == "ab";') == InterpretOk()

    assert interpret("var a = 999; assert a == 999;") == InterpretOk()
    assert interpret("var a = 999; a = a + 1; assert a == 1000;") == InterpretOk()

    assert interpret("var a = 2; { var b = a + 3; assert b == 5; }") == InterpretOk()
    assert interpret("var a = 2; { assert a == 2; }") == InterpretOk()

    assert (
        interpret(
            """
            var a = 0;
            if (1 < 2) { a = 1; }
            else       { a = 2; }
            assert a == 1;
            """
        )
        == InterpretOk()
    )

    assert (
        interpret(
            """
            var a = 0;
            if (1 > 2) { a = 1; }
            else       { a = 2; }
            assert a == 2;
            """
        )
        == InterpretOk()
    )

    assert (
        interpret(
            """
            var a = 0;
            while (a < 10) {
                a = a + 1;
            }
            assert a == 10;
            """
        )
        == InterpretOk()
    )

    assert (
        interpret(
            """
        var a  = 0;
        for(var i = 0; i<10; i=i+1) {
            a = a + 1;
        }
        assert a == 10;
        """
        )
        == InterpretOk()
    )

    assert (
        interpret(
            """  
        var a = 0;
        for(; a<10; a=a+1) {
        }
        assert a == 10;
        """
        )
        == InterpretOk()
    )

    assert (
        interpret(
            """
        fun foo(a,b) {
            return a/b;
        }
        assert foo(2,4) == 0.5;
        """
        )
        == InterpretOk()
    )

    assert (
        interpret(
            """
        fun fib(n) {
            if (n < 2) return n;
            return fib(n - 2) + fib(n - 1);
        }
        assert fib(11) == 89;
        """
        )
        == InterpretOk()
    )

    assert (
        interpret(
            """
        fun outer() {
            var a = 1;
            var b = 2;
            fun middle() {
                var c = 3;
                var d = 4;
                fun inner() {
                    return a + c + b + d;
                }
                return inner();
            }
            return middle();
        }
        assert outer() == 10;
        """
        )
        == InterpretOk()
    )

    assert (
        interpret(
            """
        var globalSet;
        var globalGet;

        fun main() {
        var a = "initial";

        fun set(value) { a = value; }
        fun get() { return a; }

        globalSet = set;
        globalGet = get;
        }

        var value = "updated";
        main();
        globalSet(value);
        assert globalGet() == value;
        """
        )
        == InterpretOk()
    )

    assert (
        interpret(
            """
        class Pair {}
        var pair = Pair();
        pair.first = 1;
        pair.second = 2;
        assert pair.first + pair.second == 3;
        """
        )
        == InterpretOk()
    )
