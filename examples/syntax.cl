type t {
    field: int;
    field_two: bool;
}

fn adder(one: int, two: int) -> int {
    return one + two;
}

fn main() {
    let var: int = 1;
    let var2: int = 2;
    print("%d", adder(var, var2));
}

