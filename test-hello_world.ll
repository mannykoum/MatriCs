; ModuleID = 'MicroC'

@fmt = private unnamed_addr constant [4 x i8] c"%d\0A\00"
@tmp = private unnamed_addr constant [7 x i8] c"hello\0A\00"

declare i32 @printf(i8*, ...)

define i32 @main() {
entry:
  %x = alloca i8*
  store i8* getelementptr inbounds ([7 x i8], [7 x i8]* @tmp, i32 0, i32 0), i8** %x
  %x1 = load i8*, i8** %x
  %printf = call i32 (i8*, ...) @printf(i8* %x1)
  ret i32 0
}
