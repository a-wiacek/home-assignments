Interpreter of imperative programming language
==============================================

Example program
---------------

```
main {
    printString("What is your name?");
    String name = getInput();
    printString("Hello, " + name + "!");
}
```

Program is list of declarations. The last one is formed as above (main {...}). Function `getInput` takes one line from stdin and returns variable of type `String`. Function `printString` writes string to stdout.

Variables
---------

```
main {
    Integer a = -(49 - 7);
    String s = "6.0";
    Double b = stringToDouble(s);
    Integer n = a / stringToInteger(doubleToString(b));
    Double x = stringToDouble(integerToString(a)) / b;
    printString(s + integerToString(a));  // 6.0-42
    printInteger(n);  // -7 
    printDouble(b);  // 6.0
}
```

Arithmetic operations need both arguments to have the same type. New variables must be initialized. Variable types are `Integer`, `Double`, `String` oraz `Bool`. Both Integers and Doubles support `+`, `-`, `*`, `/`. Integers support also `%` (second parameter must be positive). Equality operator is `==`, nonequality operator is `!=`. Strings support concatenation with `+` and `==`, `!=`. Boolean values are `True` and `False`. Bools are evaluated lazily (when we compute `a || b` and `a == True`, we dont compute `b`, same goes for `&&`). Unary minus is used also to negate booleans. There are built in functions for type conversion: `doubleToString`, `integerToString`, `stringToInteger`, `stringToDouble`. Value of expression can't be ignored.

Functions
---------

```
function f1 :: (val Integer, val Integer) -> ()
(n, d) {
    printInteger(n);
    n = n - d;
    printInteger(n);
}

function f2 :: (ref Integer, val Integer) -> ()
(n, d) {
    printInteger(n);
    n = n - d;
    return;
    printInteger(n);  // unreachable
}

function f3 :: (val Integer, val Integer) -> Integer
(n, d) {
    printInteger(n);
    n = n - d;
    printInteger(n);
    return n;
}

function f4 :: (ref Integer, val Integer) -> Integer
(n, d) {
    printInteger(n);
    n = n - d;
    return n;
}

function f5 :: () -> ()
() {
    printInteger(2);
}

main {
    Integer n = 10;
    f1(n, 2);  // 10, 8
    f2(n, 2);  // 10
    n = f3(n, 2);  // 8, 6
    Integer k = f4(n, 2);  // 6
    printInteger(k);  // 4
    printInteger(n);  // 4
    f5();  // 2
}
```

Variables can be passed either by value (`val`) or by reference (`ref`), it is required to write one of these annotaions. Functions which don't return anything have `()` as their output type. You can (but you don't have to) use `return` there. Functions returning values have to use standard `return x`.

Scope of variables
------------------

Each curly bracket has its own scope.

```
Integer n = 10;

function f :: () -> ()
() {
    printInteger(n);
}

main {
    printInteger(n);  // 10
    Integer n = 8;
    printInteger(n);  // 8
    f();  // 10
    function h :: () -> ()
    () {
        Integer n = 6;
        printInteger(n);  // 6
        function g :: () -> ()
        () {
            printInteger(n);
        }
        g();  // 6
        n = 4;
        f();  // 10
        g();  // 4
        printInteger(n);  // 4
        Integer n = 2;
        f();  // 10
        g();  // 4
        printInteger(n);  // 2
        {
            printInteger(n); // 2
            Integer n = 0;
            printInteger(n); // 0
            f(); // 10
            g(); // 4
        }
    }
    h();
    f();  // 10
    printInteger(n);  // 8
}
```

Control flow
------------
* `if (condition) then {instructions}`
* `if (condition) then {instructions} else {instructions}`
* `while (condition) do {instructions}` with `break` i `continue` allowed inside `do` block

Static variables
----------------

```
Integer x = 42;

function f :: () -> ()
() {
    static Double x = 10.0;
    x = x + 3.0;
    printDouble(x);
}

main {
    printInteger(x); // 42
    f(); // 13.0
    f(); // 16.0
    f(); // 19.0
    printInteger(x); // 42
}
```

Exceptions
----------

```
function f :: () -> ()
() {
    throw MyException;
}

// None of X will print
main {
    try {
        Integer n = 4 / 0;
        printString("X");
    } catch DivByZeroIntegerException {
        printString("A");
    }
    try {
        f();
        printString("X");
    } catch MyException {
        printString("B");
    } catch DivByZeroException {
        printString("X");
    }
}
```

Default `catch` does not exist. Namespace of exceptions and variables/functions are independent. Built in exceptions:

* `DivByZeroIntegerException`
* `DivByZeroDoubleException`
* `UnparsableNumberException` (function `stringTo*` fails to parse argument)
* `NonpositiveModuloException`
* `NoReturnException` (function is supposed to return something, but it didn't)

Compiling
=========

Required: `bnfc, happy, alex`
Build: `make` (from main directory, not `src`)
Clean: `make clean`
Run: `./interpreter "filepath"`
Interpreter doesn't have REPL mode.
Example programs are in directories `good` and `bad`.
