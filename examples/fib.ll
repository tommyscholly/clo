; ModuleID = 'jit'
source_filename = "jit"

@string_0 = private unnamed_addr constant [4 x i8] c"%d\0A\00", align 1

declare void @printf(ptr %0, ...)

define i32 @fib(i32 %i) {
entry:
  %eq = icmp eq i32 %i, 1
  %ifreturn = alloca i32, align 4
  br i1 %eq, label %then_block, label %else_block

then_block:                                       ; preds = %entry
  ret i32 1

else_block:                                       ; preds = %entry
  %eq1 = icmp eq i32 %i, 0
  %ifreturn5 = alloca i32, align 4
  br i1 %eq1, label %then_block2, label %else_block3

then_block2:                                      ; preds = %else_block
  ret i32 0

else_block3:                                      ; preds = %else_block
  %sub = sub i32 %i, 1
  %calltmp = call i32 @fib(i32 %sub)
  %fib1 = alloca i32, align 4
  store i32 %calltmp, ptr %fib1, align 4
  %sub6 = sub i32 %i, 2
  %calltmp7 = call i32 @fib(i32 %sub6)
  %fib2 = alloca i32, align 4
  store i32 %calltmp7, ptr %fib2, align 4
  %0 = load i32, ptr %fib2, align 4
  %1 = load i32, ptr %fib1, align 4
  %add = add i32 %1, %0
  %result = alloca i32, align 4
  store i32 %add, ptr %result, align 4
  %2 = load i32, ptr %result, align 4
  ret i32 %2
}

define void @main() {
entry:
  %calltmp = call i32 @fib(i32 10)
  call void (ptr, ...) @printf(ptr @string_0, i32 %calltmp)
  ret void
}
