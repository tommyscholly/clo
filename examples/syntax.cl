type t {
    field: int;
    field_two: bool;
}

fn adder(one: int, two: int) -> int {
    return one + two;
}

fn main() {
    let var: t = t { field: 15; field_two: true; };
    let var2: int = 1;
    print("%d", adder(var.field, var2));
}

