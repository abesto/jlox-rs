# jlox-rs

> Following along the first (Java) part of https://craftinginterpreters.com/, but with Rust, because reasons.

Status: done!

Check out the interpreter in action in your browser at https://abesto.github.io/jlox-rs/ (and click "What am I looking at?" for tons of details)

## Performance

About half as fast as the canonical `jlox` implementation. Considering that's Java and this is Rust, this is somewhat surprising. Considering this is the first time I ever build an interpreter, and did zero optimization, this is very surprising, but in the opposite direction.

Benchmark script:

```
fun fib(n) {
    if (n < 2) return n;
    return fib(n - 1) + fib(n - 2);
}

print fib(30);
```

Benchmark results:

```
$ hyperfine --warmup 1 '../craftinginterpreters/jlox ./fib.lox' './target/release/jlox_rs ./fib.lox'
Benchmark 1: ../craftinginterpreters/jlox ./fib.lox
  Time (mean ± σ):     855.8 ms ± 196.5 ms    [User: 1134.4 ms, System: 106.2 ms]
  Range (min … max):   695.4 ms … 1151.9 ms    10 runs
 
Benchmark 2: ./target/release/jlox_rs ./fib.lox
  Time (mean ± σ):      2.016 s ±  0.026 s    [User: 2.011 s, System: 0.002 s]
  Range (min … max):    1.982 s …  2.057 s    10 runs
 
Summary
  '../craftinginterpreters/jlox ./fib.lox' ran
    2.36 ± 0.54 times faster than './target/release/jlox_rs ./fib.lox'
```

## Correctness

I added my own tests as I went, and then at the very end found that https://github.com/munificent/craftinginterpreters contains a full test suite. Running it against this implementation, I see 100 passes and 139 failures. A ton of the failures are due to error message formatting. A handful might not be, but honestly that's fine for this hobby project. I'll probably use those tests for `clox-rs` though!
