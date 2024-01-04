fn fib(i: int) -> int {
    if i == 1 {
        return 1;
    } else {
        if i == 0 {
            return 0;
        } else {
            let fib1 = fib(i - 1);
            let fib2 = fib(i - 2);
            let result = fib1 + fib2;
            return result;
        };
    };
}

fn main() {
    // trivial iterative for loop
    for i in 1..10 {
        print("%d\n", fib(i));
    };
}
