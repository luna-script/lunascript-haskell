define external ccc  i32 @fib(i32  %n_0)    {
; <label>:0:
  %1 = icmp eq i32 %n_0, 1
  br i1 %1, label %then_0, label %else_0
then_0:
  br label %end_1
else_0:
  %2 = icmp eq i32 %n_0, 2
  br i1 %2, label %then_1, label %else_1
then_1:
  br label %end_0
else_1:
  %3 = sub   i32 %n_0, 1
  %4 =  call ccc  i32  @fib(i32  %3)
  %5 = sub   i32 %n_0, 2
  %6 =  call ccc  i32  @fib(i32  %5)
  %7 = add   i32 %4, %6
  br label %end_0
end_0:
  %8 = phi i32 [1, %then_1], [%7, %else_1]
  br label %end_1
end_1:
  %9 = phi i32 [1, %then_0], [%8, %end_0]
  ret i32 %9
}


define external ccc  i32 @main()    {
  %1 =  call ccc  i32  @fib(i32  5)
  ret i32 %1
}
