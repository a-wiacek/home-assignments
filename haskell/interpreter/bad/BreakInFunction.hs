main {
    Integer n = 3;
    while (n > 0) do {
        function f :: () -> ()
        () {
            printInteger(n);
            break;
        }
        f();
        n = n - 1;
    }
}
