struct T {
    field: int;
    field_two: bool;
}

enum E {
    Int(int),
    Bool(bool)
}

fn adder(one: int, two: int) -> int {
    return one + two;
}

fn main() {
    let var = T { field: 15; field_two: true; };
    let var2 = 1;
    print("%d", adder(var.field, var2));
}

