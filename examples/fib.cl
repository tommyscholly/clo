fn fib(i: int) -> int {
    print("here");
    if i == 1 {
        return 1;
    }

    return i;
}

fn main() {
    let fib_result = fib(4);
    print("%d\n", fib_result);
    print("here2");
}
