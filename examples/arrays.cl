fn main() {
    let basic_array = [4] { 1, 2, 3, 4 };

    for i in 0..3 {
        print("%d\n", basic_array[i]);
    };

    for i in 0..10 {
        // if i == 5 {
            // break;
        // };
        break;

        print("iter: %d\n", i);
    };
}
