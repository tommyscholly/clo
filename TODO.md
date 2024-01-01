DONE: Type check struct field definition
  - i.e if you have type 'type t { one: int }'
  - you can define it as 'let test: t = t { one: true }'
  - and it generates "valid" LLVM IR, as LLVM will willingly write an i8 to an i32

DONE: convert field access to use build/load

DONE: assignments

Type inference
  - now that we can determine the type of any function, we can have basic type inference
  - DONE: basic variable type inference

Type check expressions passed to functions
  - currently LLVM is type checking that, which results in a segfault if incorrect args are passed

Tagged unions
  - mostly done i believe?

if statements
  - now that match statements are done, an 'if let' statement should be relatively easy to do


loops

