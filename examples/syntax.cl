struct T {
    field: int;
    field_two: bool;
}

enum E {
    Int(int),
    Bool(bool),
    Struct {
        field: int;
        other: int;
        other_other: int;
        b: int;
    },
    None
}

fn matcher(thing: E) {
    match thing with
    | E:Int(i) -> print("%d", i),
    | E:Bool(b) -> print("%d", b),
    | E:Struct s -> print("%d, %d, %d, %d", s.field, s.other, s.other_other, s.b),
    | E:None -> print("None");
}

fn adder(one: int, two: int) -> int {
    return one + two;
}

fn main() {
    let var = T { field: 15; field_two: true; };
    let e = E:Int(1);
    let other = E:Struct {
        field: 1;
        other: 2;
        other_other: 3;
        b: 4;
    };
    let var2 = 1;
    matcher(e);
    print("%d", adder(var.field, var2));
}

