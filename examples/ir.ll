; ModuleID = 'jit'
source_filename = "jit"

%"E:Int" = type { i8, i32 }
%"E:Bool" = type { i8, i8 }
%"E:Struct" = type { i8, i32, i32, i32, i32 }
%T = type { i32, i8 }

@string_0 = private unnamed_addr constant [3 x i8] c"%d\00", align 1
@string_1 = private unnamed_addr constant [15 x i8] c"%d, %d, %d, %d\00", align 1
@string_2 = private unnamed_addr constant [5 x i8] c"None\00", align 1

declare void @printf(ptr %0, ...)

define void @matcher(ptr %thing) {
entry:
  %case = load i32, ptr %thing, align 4
  switch i32 %case, label %default [
    i32 0, label %"E:Int_case"
    i32 1, label %"E:Bool_case"
    i32 2, label %"E:Struct_case"
    i32 3, label %"E:None_case"
  ]

default:                                          ; preds = %entry
  unreachable

"E:Int_case":                                     ; preds = %entry
  %i = getelementptr inbounds %"E:Int", ptr %thing, i32 0, i32 1
  call void (ptr, ...) @printf(ptr @string_0, ptr %i)
  br label %match_finish

"E:Bool_case":                                    ; preds = %entry
  %b = getelementptr inbounds %"E:Bool", ptr %thing, i32 0, i32 1
  call void (ptr, ...) @printf(ptr @string_0, ptr %b)
  br label %match_finish

"E:Struct_case":                                  ; preds = %entry
  %"1" = getelementptr inbounds %"E:Struct", ptr %thing, i32 0, i32 1
  %loadfield = load i32, ptr %"1", align 4
  %"2" = getelementptr inbounds %"E:Struct", ptr %thing, i32 0, i32 2
  %loadfield1 = load i32, ptr %"2", align 4
  %"3" = getelementptr inbounds %"E:Struct", ptr %thing, i32 0, i32 3
  %loadfield2 = load i32, ptr %"3", align 4
  %"4" = getelementptr inbounds %"E:Struct", ptr %thing, i32 0, i32 4
  %loadfield3 = load i32, ptr %"4", align 4
  call void (ptr, ...) @printf(ptr @string_1, i32 %loadfield, i32 %loadfield1, i32 %loadfield2, i32 %loadfield3)
  br label %match_finish

"E:None_case":                                    ; preds = %entry
  call void (ptr, ...) @printf(ptr @string_2)
  br label %match_finish

match_finish:                                     ; preds = %"E:None_case", %"E:Struct_case", %"E:Bool_case", %"E:Int_case"
  ret void
}

define i32 @adder(i32 %one, i32 %two) {
entry:
  %addtmp = add i32 %one, %two
  ret i32 %addtmp
}

define void @main() {
entry:
  %T = alloca %T, align 8
  %"0" = getelementptr inbounds %T, ptr %T, i32 0, i32 0
  store i32 15, ptr %"0", align 4
  %"1" = getelementptr inbounds %T, ptr %T, i32 0, i32 1
  store i8 1, ptr %"1", align 1
  %var = alloca ptr, align 8
  store ptr %T, ptr %var, align 8
  %E = alloca %"E:Int", align 8
  %enum_tag_idx = getelementptr inbounds %"E:Int", ptr %E, i32 0, i32 0
  store i8 0, ptr %enum_tag_idx, align 1
  %enum_data = getelementptr inbounds %"E:Int", ptr %E, i32 0, i32 1
  store i32 1, ptr %enum_data, align 4
  %e = alloca ptr, align 8
  store ptr %E, ptr %e, align 8
  %E1 = alloca %"E:Struct", align 8
  %enum_tag_idx2 = getelementptr inbounds %"E:Struct", ptr %E1, i32 0, i32 0
  store i8 2, ptr %enum_tag_idx2, align 1
  %enum_field = getelementptr inbounds %"E:Struct", ptr %E1, i32 0, i32 1
  store i32 1, ptr %enum_field, align 4
  %enum_field3 = getelementptr inbounds %"E:Struct", ptr %E1, i32 0, i32 2
  store i32 2, ptr %enum_field3, align 4
  %enum_field4 = getelementptr inbounds %"E:Struct", ptr %E1, i32 0, i32 3
  store i32 3, ptr %enum_field4, align 4
  %enum_field5 = getelementptr inbounds %"E:Struct", ptr %E1, i32 0, i32 4
  store i32 4, ptr %enum_field5, align 4
  %other = alloca ptr, align 8
  store ptr %E1, ptr %other, align 8
  %var2 = alloca i32, align 4
  store i32 1, ptr %var2, align 4
  call void @matcher(ptr %E)
  %"06" = getelementptr inbounds %T, ptr %T, i32 0, i32 0
  %loadfield = load i32, ptr %"06", align 4
  %calltmp = call i32 @adder(i32 %loadfield, i32 1)
  call void (ptr, ...) @printf(ptr @string_0, i32 %calltmp)
  ret void
}
