; ModuleID = 'main'





define external ccc  i32 @main()    {
; <label>:0:
  %1 = icmp slt i32 4, 3
  br i1 %1, label %then_0, label %else_0
then_0:
  br label %end_1
else_0:
  %2 = icmp slt i32 4, 3
  br i1 %2, label %then_1, label %else_1
then_1:
  br label %end_0
else_1:
  br label %end_0
end_0:
  %3 = phi i32 [6, %then_1], [7, %else_1]
  br label %end_1
end_1:
  %4 = phi i32 [5, %then_0], [%3, %end_0]
  ret i32 %4
}
