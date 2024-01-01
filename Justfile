compile:
    #!/usr/bin/env zsh
    dune exec bin/main.exe examples/syntax.cl &> just_out.ll
    llvm-as-16 -o just_out.bc just_out.ll
    llc-16 -filetype=obj -o just_out.o just_out.bc
    gcc -o out just_out.o -no-pie
