#!/bin/bash
assert() {
  expected="$1"
  input="$2"

  echo "$input" | stack run | lli
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
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
assert 4 "let succ(n) = n + 1;
let double(f, n) = f(f(n));
let main = double(succ, 2);"

echo OK
