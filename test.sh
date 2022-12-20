#!/bin/bash
stack install
result=true
assert() {
  expected="$1"
  input="$2"

  echo "$input" | lunascript-haskell-exe | lli
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo -e "$input => \e[32m$actual\e[m"
  else
    echo -e "$input => \e[31m$expected expected, but got $actual\e[m"
    result=false
  fi
}

assertStdOut() {
  expected="$1"
  input="$2"

  actual=`echo "$input" | stack run | lli`

  if [ "$actual" = "$expected" ]; then
    echo -e "$input => \e[32m$actual\e[m"
  else
    echo -e "$input => \e[31m$expected expected, but got $actual\e[m"
    result=false
  fi
}

assert 10 "let main = 10;"
assert 5 "let main = 2 + 3;"
assert 5 "let main = 8 - 3;"
assert 10 "let main = 2 * 5;"
assert 2 "let main = 4 / 2;"
assert 10 "let main = 1 + 3 * 3;"
assert 12 "let main = (1 + 3) * 3"
assert 0 "let main = False;"
assert 1 "let main = True;"
assert 1 "let main = 2 == 2;"
assert 0 "let main = 2 == 3;"
assert 1 "let main = 2 < 3;"
assert 0 "let main = 3 < 2;"
assert 2 "let main = if (True) 2 else 3;"
assert 3 "let main = if (False) 2 else 3;"
assert 3 "let a = 1; let main = a + 2;"
assert 3 "let a() = 1; let main = a() + 2;"
assert 5 "let add(a, b) = a + b; let main = add(2, 3);"
assert 5 "let a = 2; let b = 3; let add(a, b) = a + b; let main = add(a, b);"
assert 6 "let fact(n) = if (n == 1) 1 else n * fact(n - 1); let main = fact(3);"
assert 5 "let fib(n) = if (n == 1) 1 else if (n == 2) 1 else fib(n - 1) + fib(n - 2); let main = fib(5);"
assert 8 "let a = {let b = 2; b + b};
let main = a + a;"
assert 4 "let a = {let b = 2; b + {let b = 1; b + b}};
let main = a;"
assert 13 "let func(a, b) = {
    let c = a + b;
    a * c + b
};
let main = func(2, 3);"
assert 4 "let main = if (2 < 3) { 
    let a = 1;
    a + 3
} else { 
    let a = 2;
    a + 3
};"
assert 4 "let succ(n) = n + 1;
let double(f, n) = f(f(n));
let main = double(succ, 2);"
assert 5 "let sub = fn a, b -> a - b;
let main = sub(7, 2);"
assert 5 "let sub = fn a -> fn b -> a - b;
let main = sub(9, 4);"
assert 5 "let sub(a) = fn b -> a - b;
let main = sub(8, 3);"
assertStdOut 1 "let main = print_int(1);"
assertStdOut -1 "let main = print_int(1-2);"
assertStdOut -1 "let main = print_int(-1);"
assertStdOut -3 "let main = print_int(-1 + -2);"
assertStdOut 1 "let main = if (2<3) print_int(1) else print_int(2);"
assert 3 "let main = -1 * -3;"
assert 5 "let main = a; let a = 5;"
assert 5 "let main = a(); let a() = 5;"
assert 5 "let main = add(2, 3); let add(a, b) = a + b;"
assert 1 "let main = isOdd(3);
let isOdd(n) = if (n==0) False else isEven(n - 1);
let isEven(n) = if (n==0) True else isOdd(n - 1);"
assert 6 "let main = {
    let a = [1, 2, 3];
    a[0] + a[1] + a[2]
};"
assert 6 "let a = [1, 2, 3];
let main = a[0] + a[1] + a[2];"
assert 35 "let a = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,35];
let main = a[34]"
assert 6 "let add(a, b) = a + b; let main = foldl(add, 0, [1, 2, 3]);"
assert 6 "let add(a, b) = a + b;
let a = [1, 2, 3];
let main = foldl(add, 0, a);"
assert 3 "let lambda(acm, n) = acm + 1;
let mylength(vec) = foldl(lambda, 0, vec);
let main = mylength([1, 2, 3]);"
assert 3 "let lambda(acm, n) = acm + 1;
let mylength(vec) = foldl(lambda, 0, vec);
let vec = [1, 2, 3];
let main = mylength(vec);"
assert 35 "let lambda(acm, n) = acm + 1;
let mylength(vec) = foldl(lambda, 0, vec);
let main = mylength([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]);"
assert 35 "let lambda(acm, n) = acm + 1;
let mylength(vec) = foldl(lambda, 0, vec);
let vec = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1];
let main = mylength(vec);"
assert 35 "let add(acm, n) = acm + n;
let sum(vec) = foldl(add, 0, vec);
let main = sum([1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1]);"
assert 35 "let add(acm, n) = acm + n;
let sum(vec) = foldl(add, 0, vec);
let vec = [1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1];
let main = sum(vec);"
assert 6 "let main = foldl(fn a, b -> a + b, 0, [1, 2, 3]);"
assert 6 "let main = { 
    let add(a, b) = a + b;
    foldl(add, 0, [1, 2, 3])
};"
assert 0 "let main = [True, False][1];"
assert 3 "let main = [[3, 0], [0, 1]][0][0];"
assert 3 "let main = [[3, 0], [0, 1]][0, 0];"
assert 2 "let a = [[1, 0], [0, 1]];
let main = a[0, 0] + a[1, 1];"
assert 2 "let main = length([[1, 0], [0, 1]]);"
assert 2 "let mylength(vec) = foldl(fn acm, n -> acm + 1, 0, vec);
let main = mylength([True, False]);"
assert 10 "let a = [[1, 2], [3, 4]]; 
let sum(vec) = foldl(fn a, b -> a + b, 0, vec);
let main = sum(a[0])+sum(a[1]);"
assert 5 "let get = 5; let main = get;"
assert 5 "let main = get; let get = 5; "
assert 5 "let main = {
    let a = 1;
    let! a = a + 4;
    a
}"
assert 5 "let main = {
    let a = 5;
    let! a = a;
    a
}"
assert 5 "let main = {
    let add(a, b) = a + b;
    let! add = add;
    add(2, 3)
}"
assert 10 "
add : Int -> Int -> Int
let add(a, b) = a + b;

main : Int
let main = add(4, 6);
"
assert 10 "
hoge : Int -> Bool -> Int
let hoge(a, b) = a;

main : Int
let main = hoge(10, True);
"
assert 6 "
let main = fact(3);
let fact(n) = {
    let go(n, result) = if (n==0) result else go(n-1, n*result);
    go(n, 1)
}"
assert 6 "let fact(n) = {
    let go(n, result) = if (n==0) result else go(n-1, n*result);
    go(n, 1)
};
let main = fact(3);
"
assert 0 "let id(n) = n;
let main = id(id(1)==0);"
assert 0 "let id(n) = n;
let id2(n) = id(n);
let main = id2(id2(1)==0);"
assert 4 "let main = {
  let a = ref(4);
  *a
}"
assert 10 "let main = {
  let a = ref(4);
  a := *a + 6;
  *a
}"
assert 8 "let main = {
  let a = ref(4);
  a := 2 * *a;
  *a
}"
assert 6 "
let func(ref) = {
  ref := *ref * 2
};
let main = {
  let a = ref(3);
  func(a);
  *a
}
"
assertStdOut 3 "
let main = {
  let a = ref(3);
  print_int(*a)
}"
assertStdOut 3 "
let func(r) = {
  print_int(*r)
};
let main = {
  let a = ref(3);
  func(a)
}"
